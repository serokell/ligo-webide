import AWS from 'aws-sdk/global'
import S3 from 'aws-sdk/clients/s3'
import path from 'path-browserify'

import { AWSS3Region, AWSBucket } from './config.json'

const region = process.env.REACT_APP_AWS_S3_REGION || AWSS3Region
const Bucket = process.env.REACT_APP_AWS_BUCKET || AWSBucket

export default class AwsS3Fs {
  constructor () {
    this.promises = {
      readFile: this.readFile.bind(this),
      writeFile: this.writeFile.bind(this),
      stat: this.stat.bind(this),
      ensureFile: this.ensureFile.bind(this)
    }
  }

  updateCredential (credential) {
    AWS.config.update({
      region,
      accessKeyId: credential.Credentials.AccessKeyId,
      secretAccessKey: credential.Credentials.SecretAccessKey,
      sessionToken: credential.Credentials.SessionToken
    })

    this.s3 = new S3()
  }

  async readFile (filePath, { encoding } = {}) {
    // if (filePath.startsWith('/')) {
    //   filePath = filePath.substr(1)
    // }
    // const params = {
    //   Bucket,
    //   Key: filePath
    // }
    // const result = await this.s3.getObject(params).promise()
    // return result.Body.toString(encoding)

    // cb = cb || function () { /* do nothing. */ }
    // path = this.getPathFromUrl(path) || path // ensure we actually use the normalized path from here
    // var unprefixedpath = this.removePrefix(path)
    try {
      const content = await window.remixFileSystem.readFile(filePath, 'utf8')
      // if (cb) cb(null, content)
      return content
    } catch (err) {
      // if (cb) cb(err, null)
      throw new Error(err)
    }
  }

  async createDir (path, cb) {
    const unprefixedpath = path
    const paths = unprefixedpath.split('/')
    if (paths.length && paths[0] === '') paths.shift()
    let currentCheck = ''
    for (const value of paths) {
      currentCheck = currentCheck + '/' + value
      if (!await window.remixFileSystem.exists(currentCheck)) {
        try {
          await window.remixFileSystem.mkdir(currentCheck)
        } catch (error) {
          console.log(error)
        }
      }
    }
    // currentCheck = ''
    // for (const value of paths) {
    //   currentCheck = currentCheck + '/' + value
    //   this.event.emit('folderAdded', this._normalizePath(currentCheck))
    // }
    // if (cb) cb()
  }

  async writeFile (filePath, content) {
    // const params = {
    //   Bucket,
    //   Key: filePath,
    //   Body: content
    // }
    // await this.s3.putObject(params).promise()

    var unprefixedpath = filePath
    const exists = await window.remixFileSystem.exists(unprefixedpath)
    if (exists && await window.remixFileSystem.readFile(unprefixedpath, 'utf8') === content) {
      return null
    }
    await this.createDir(unprefixedpath.substr(0, unprefixedpath.lastIndexOf('/')))
    try {
      await window.remixFileSystem.writeFile(unprefixedpath, content, 'utf8')
    } catch (e) {
      return false
    }
    return true
  }

  async ensureFile (filePath) {
    return this.writeFile(filePath, '')
  }

  async ensureDir (filePath) {
    return this.writeFile(`${filePath}/.placeholder`)
  }

  async rename (oldPath, newPath) {
    var unprefixedoldPath = oldPath
    var unprefixednewPath = newPath
    if (await window.remixFileSystem.exists(unprefixedoldPath)) {
      await window.remixFileSystem.rename(unprefixedoldPath, unprefixednewPath)
      return true
    }
    return false

    // if (oldPath === newPath) {
    //   return
    // }
    // const isFile = !oldPath.endsWith('/')

    // if (isFile) {
    //   let fileExists = false
    //   try {
    //     await this.readFile(newPath, {})
    //     fileExists = true
    //   } catch (error) {
    //     fileExists = false
    //   }
    //   if (fileExists) {
    //     throw new Error(`${newPath} exists.`)
    //   }

    //   const oldParams = {
    //     CopySource: `/${Bucket}/${oldPath}`,
    //     Bucket,
    //     Key: newPath
    //   }
    //   await this.s3.copyObject(oldParams).promise()
    //   await this.deleteFile(oldPath)
    // } else {
    //   const listParams = {
    //     Bucket,
    //     Prefix: oldPath,
    //     Delimiter: '/'
    //   }
    //   const listResult = await this.s3.listObjectsV2(listParams).promise()
    //   const promises = listResult.Contents.map(async ({ Key }) => {
    //     if (Key === oldPath) {
    //       return
    //     }
    //     return this.rename(Key, Key.replace(oldPath, newPath))
    //   })
    //   await Promise.all(promises)
    //   await this.deleteFolder(oldPath)
    // }
  }

  async deleteFile (filePath) {
    const path = filePath
    if (await window.remixFileSystem.exists(path) && !(await window.remixFileSystem.stat(path)).isDirectory()) {
      await window.remixFileSystem.unlink(path)
      return true
    } else return false
    // const params = {
    //   Bucket,
    //   Key: filePath
    // }
    // await this.s3.deleteObject(params).promise()
  }

  async deleteFolder (dirPath) {
    const path = dirPath
    if (await window.remixFileSystem.exists(path)) {
      const stat = await window.remixFileSystem.stat(path)
      try {
        if (!stat.isDirectory()) {
          return (this.deleteFile(path))
        } else {
          const items = await window.remixFileSystem.readdir(path)
          if (items.length !== 0) {
            for (const item of items) {
              const curPath = `${path}${path.endsWith('/') ? '' : '/'}${item}`
              if ((await window.remixFileSystem.stat(curPath)).isDirectory()) { // delete folder
                await this.deleteFolder(curPath)
              } else { // delete file
                await this.deleteFile(curPath)
              }
            }
            await window.remixFileSystem.rmdir(path)
          } else {
            // folder is empty
            await window.remixFileSystem.rmdir(path)
          }
        }
      } catch (e) {
        console.log(e)
        return false
      }
    }
    // await this.emptyS3Directory(dirPath)
    // const params = {
    //   Bucket,
    //   Key: `${dirPath}/`
    // }
    // await this.s3.deleteObject(params).promise()
  }

  // async emptyS3Directory(dirPath) {
  //   const listedObjects = await this.s3.listObjectsV2({
  //     Bucket,
  //     Prefix: dirPath
  //   }).promise()
  //   if (listedObjects.Contents.length === 0) {
  //     return
  //   }

  //   const deleteParams = {
  //     Bucket,
  //     Delete: { Objects: [] }
  //   }
  //   listedObjects.Contents.forEach(({ Key }) => {
  //     deleteParams.Delete.Objects.push({ Key })
  //   })
  //   await this.s3.deleteObjects(deleteParams).promise()

  //   if (listedObjects.IsTruncated) {
  //     await this.emptyS3Directory(dirPath)
  //   }
  // }

  async exists (path) {
    return await window.remixFileSystem.exists(path)
  }

  async stat (fileOrDirPath) {
    // if (fileOrDirPath.startsWith('/')) {
    //   fileOrDirPath = fileOrDirPath.substr(1)
    // }
    // const { dir, base } = path.parse(fileOrDirPath)
    // const list = await this.list(dir)
    // const match = list.find(item => item.name === base)
    return {
      isDirectory: async () => (await window.remixFileSystem.stat(fileOrDirPath)).isDirectory(),
      isFile: async () => (await window.remixFileSystem.stat(fileOrDirPath)).isFile()
    }
  }

  async resolveDirectory (path) {
    // path = this.removePrefix(path)
    console.log(JSON.stringify(path))
    if (path.indexOf('/') !== 0) path = '/' + path
    try {
      const files = await window.remixFileSystem.readdir(path)
      const ret = {}
      if (files) {
        for (let element of files) {
          path = path.replace(/^\/|\/$/g, '') // remove first and last slash
          element = element.replace(/^\/|\/$/g, '') // remove first and last slash
          const absPath = (path === '/' ? '' : path) + '/' + element
          ret[absPath.indexOf('/') === 0 ? absPath.substr(1, absPath.length) : absPath] = { isDirectory: (await window.remixFileSystem.stat(absPath)).isDirectory() }
          // ^ ret does not accept path starting with '/'
        }
      }
      return ret
    } catch (error) {
      console.log(JSON.stringify(error))
    }
  }

  async list (dirPath) {
    console.log(JSON.stringify(dirPath))
    const result = await this.resolveDirectory(dirPath)

    const folders = Object.keys(result).filter(item => result[item].isDirectory).map(item => {
      const path = item
      const name = path.replace(`${dirPath}/`, '')
      return { type: 'folder', title: name, key: path, children: [], isLeaf: false, name, path, loading: true, remote: true }
    })

    const files = Object.keys(result).filter(item => !result[item].isDirectory).map(item => {
      const path = item
      const name = path.replace(`${dirPath}/`, '')
      return { type: 'file', title: name, key: path, name, path, remote: true, isLeaf: true }
    })

    return [...folders, ...files]
  }
}

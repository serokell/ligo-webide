export default class WebLocalFs {
  constructor () {
    this.promises = {
      readFile: this.readFile.bind(this),
      writeFile: this.writeFile.bind(this),
      stat: this.stat.bind(this),
      ensureFile: this.ensureFile.bind(this)
    }
  }

  async readFile (filePath, cb) {
    try {
      const content = await window.remixFileSystem.readFile(filePath, 'utf8')
      if (cb) cb(null, content)
      return content
    } catch (err) {
      if (cb) cb(err, null)
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
  }

  async writeFile (filePath, content) {
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
  }

  async deleteFile (filePath) {
    const path = filePath
    if (await window.remixFileSystem.exists(path) && !(await window.remixFileSystem.stat(path)).isDirectory()) {
      await window.remixFileSystem.unlink(path)
      return true
    } else return false
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
  }

  async exists (path) {
    return await window.remixFileSystem.exists(path)
  }

  async stat (fileOrDirPath) {
    return {
      isDirectory: async () => (await window.remixFileSystem.stat(fileOrDirPath)).isDirectory(),
      isFile: async () => (await window.remixFileSystem.stat(fileOrDirPath)).isFile()
    }
  }

  async resolveDirectory (path) {
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

import path from 'path-browserify'
import FileOps from './FileOps'
import WebLocalFs from './WebLocalFs'

export default class WebLocalFileOps extends FileOps {
  constructor () {
    const fs = new WebLocalFs()
    super(fs, path)
  }

  async listFolder (folderPath) {
    return await this.fs.list(folderPath)
  }

  showMessageBox ({ message, buttons }) {
    const result = window.confirm(message)
    return { response: result ? 0 : 1 }
  }

  async createNewFolder (folderPath) {
    try {
      await this.fs.ensureDir(folderPath)
    } catch (e) {
      throw new Error(`Fail to create the folder <b>${folderPath}</b>.`)
    }
  }

  getAppVersion () {
    return process.env.APP_VERSION
  }

  openLink (href) {
    window.open(href, '_blank')
  }

  deleteFile (filePath) {
    return this.fs.deleteFile(filePath)
  }

  deleteFolder (filePath) {
    return this.fs.deleteFolder(filePath)
  }
}

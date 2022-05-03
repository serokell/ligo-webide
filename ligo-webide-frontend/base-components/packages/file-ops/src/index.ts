import ElectronFileOps from './ElectronFileOps'
import WebFileOps from './WebFileOps'
import WebLocalFileOps from './WebLocalFileOps'

class FileOpsManager {
  constructor () {
    this._fsType = null
    this._fileOps
    this.web = new WebFileOps()
  }

  set fsType (fsType) {
    if (fsType === 'electron') {
      this._fsType = fsType
      this._fileOps = new ElectronFileOps()
      return
    } else if (fsType === 'web') {
      this._fsType = fsType
      // this._fileOps = this.web TODO replace or remove web file ops
      this._fileOps = new WebLocalFileOps()
      return
    }
    throw new Error(`Unknown fsType "${this.fsType}".`)
  }

  get fsType () {
    return this._fsType
  }

  get current () {
    if (!this._fsType) {
      throw new Error('this.fsType is not defined.')
    }
    if (!this._fileOps) {
      throw new Error(`Unknown fsType "${this.fsType}".`)
    }
    return this._fileOps
  }
}

export default new FileOpsManager()

export { fileSystems, fileSystem } from './filesystems/fileSystem'
export { IndexedDBStorage, indexedDBFileSystem } from './filesystems/indexedDB'

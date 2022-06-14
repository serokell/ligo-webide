import React, { PureComponent } from 'react'

import {
  Modal,
  DebouncedFormGroup
} from '@obsidians/ui-components'

import notification from '@obsidians/notification'

import ProjectManager from '../ProjectManager'
import actions from '../actions'

export default class OpenProjectModal extends PureComponent {
  constructor (props) {
    super(props)

    this.state = {
      name: '',
      link: '',
      loading: ''
    }

    this.modal = React.createRef()
    this.nameInput = React.createRef()
    this.linkInput = React.createRef()

    actions.openProjectModal = this
  }

  openModal () {
    this.setState({
      name: '',
      link: '',
      loading: false
    })
    this.forceUpdate()
    this.modal.current.openModal()
    return new Promise(resolve => { this.onConfirm = resolve })
  }

  onOpenProject = async () => {
    this.setState({ creating: true })

    const { name, link } = this.state

    const gistId = this.getGistId(link)

    let data
    try {
      data = await (await fetch(`https://api.github.com/gists/${gistId}`)).json()
      if (!data.files) {
        notification.error('Gist load error', data.message)
        this.setState({ creating: false })
        return
      }
    } catch (e) {
      notification.error('Gist load error', e.message)
      this.setState({ creating: false })
      return
    }

    const obj = {}
    Object.keys(data.files).forEach((element) => {
      const path = element.replace(/\.\.\./g, '/')
      obj[path] = data.files[element]
    })

    const created = await this.openProject(obj, name)

    if (created) {
      this.modal.current.closeModal()
      this.onConfirm(created)
      this.setState({ name: '', link: '', loading: false })
    } else {
      this.setState({ creating: false })
    }
  }

  getGistId = (str) => {
    const idr = /[0-9A-Fa-f]{8,}/
    const match = idr.exec(str)
    return match ? match[0] : null
  }

  async openProject (obj, name) {
    try {
      const Manager = ProjectManager.Local
      const created = await Manager.openProject(obj, name)
      notification.success('Successful', `New project <b>${name}</b> is loaded.`)
      return created
    } catch (e) {
      notification.error('Cannot Create the Project', e.message)
      return false
    }
  }

  render () {
    return (
      <Modal
        ref={this.modal}
        title='Load project from Gist'
        textConfirm='Load'
        onConfirm={this.onOpenProject}
        pending={this.state.loading && 'Loading...'}
        confirmDisabled={!this.state.name || !this.state.link}
      >
        <DebouncedFormGroup
          ref={this.nameInput}
          label='Project name'
          value={this.state.name}
          onChange={name => this.setState({ name })}
        />
        <DebouncedFormGroup
          ref={this.linkInput}
          label='Gist link'
          value={this.state.link}
          onChange={link => this.setState({ link })}
        />
      </Modal>
    )
  }
}

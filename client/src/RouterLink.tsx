import * as React from 'react';

export class RouterLink extends React.Component<{path: string, disabled?: boolean}, {}> {
  render() {
    const action = this.props.disabled
      ? (e: React.SyntheticEvent) => e.preventDefault()
      : (e: React.SyntheticEvent) => {e.preventDefault(); window.history.pushState(this.props.path, this.props.path, this.props.path)}

    const className = this.props.disabled? 'disabled' : undefined

    return <a href={this.props.path} className={className} onClick={action}>{this.props.children}</a>
  }
}
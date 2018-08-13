import * as React from 'react';

export class Link extends React.Component<{action: (e: React.SyntheticEvent) => void, disabled?: boolean}, {}> {
  render() {
    const action = this.props.disabled
      ? (e: React.SyntheticEvent) => e.preventDefault()
      : (e: React.SyntheticEvent) => {e.preventDefault(); this.props.action(e)}

    const className = this.props.disabled? 'disabled' : undefined

    return <a href="#" className={className} onClick={action}>{this.props.children}</a>
  }
}
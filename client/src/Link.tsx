import * as React from 'react';

export class Link extends React.Component<{action: (e: React.SyntheticEvent) => void, disabled?: boolean}, {}> {
  render() {
    let action = this.props.disabled
      ? (e: React.SyntheticEvent) => e.preventDefault()
      : (e: React.SyntheticEvent) => {e.preventDefault(); this.props.action(e)}

    return <a href="#" onClick={action}>{this.props.children}</a>
  }
}
import * as React from 'react';

export class Link extends React.Component<{action: () => void}, {}> {
  render() {
    return <a href="#" onClick={e => {e.preventDefault(); this.props.action()}}>{this.props.children}</a>
  }
}
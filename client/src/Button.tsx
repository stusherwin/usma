import * as React from 'react';

export class Button extends React.Component<{action: (e: React.SyntheticEvent) => void, disabled?: boolean}, {}> {
  render() {
    return <button disabled={this.props.disabled} onClick={(e) => this.props.action(e)}>{this.props.children}</button>
  }
}
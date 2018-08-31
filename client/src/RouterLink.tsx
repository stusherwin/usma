import * as React from 'react';

export interface RouterLinkProps extends React.DetailedHTMLProps<React.AnchorHTMLAttributes<HTMLAnchorElement>, HTMLAnchorElement> {
  path: string
}

export class RouterLink extends React.Component<RouterLinkProps, {}> {
  render() {
    const action = (e: React.SyntheticEvent) => {e.preventDefault(); window.history.pushState(this.props.path, this.props.path, this.props.path)}
    const {href, onClick, path, ...rest} = this.props

    return <a href={this.props.path} {...rest} onClick={action}>{this.props.children}</a>
  }
}
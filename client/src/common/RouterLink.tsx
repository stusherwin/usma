import * as React from 'react';

export interface RouterLinkProps extends React.DetailedHTMLProps<React.AnchorHTMLAttributes<HTMLAnchorElement>, HTMLAnchorElement> {
  path: string
}

export class RouterLink extends React.Component<RouterLinkProps, {}> {
  render() {
    const groupPart = window.location.href.split('/').filter(l => l.length).slice(2, 4).join('/')
    const url = '/' + groupPart + this.props.path
    const action = (e: React.SyntheticEvent) => {e.preventDefault(); window.history.pushState(url, url, url)}
    const {href, onClick, path, ...rest} = this.props

    return <a href={url} {...rest} onClick={action}>{this.props.children}</a>
  }
}
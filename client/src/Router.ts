import * as React from 'react';

export class Router {
  private routes: {route: string, component: (c: {[key: string]: string}) => JSX.Element}[] = []
  
  route(route: string, component: (c: {[key: string]: string}) => JSX.Element) {
    this.routes.push({route, component})
  }

  resolve(url: string): JSX.Element {
    for(let r of this.routes) {
      let identifierRegExp = /\{([^\}]+)\}/gi
      let identifiers = identifierRegExp.exec(r.route)
      let routeRegExp = new RegExp('^' + r.route.replace(identifierRegExp, '([^/]+)'), "gi")
      let matches = routeRegExp.exec(url)
      if(!matches) {
        continue;
      }

      let identifierValues: {[key: string]: string} = {}
      if(identifiers && identifiers.length > 1 && matches && matches.length > 1) {
        for(let i = 1; i < identifiers.length; i++) {
          identifierValues[identifiers[i]] = matches[i]
        }
      }
      return r.component(identifierValues)
    }
    return React.createElement('div', null, 'Page not found')
  }
}
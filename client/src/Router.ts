import * as React from 'react';

export class Router {
  private routes: {route: string, component: (c: {[key: string]: string}) => JSX.Element}[] = []
  
  route(route: string, component: (c: {[key: string]: string}) => JSX.Element) {
    this.routes.push({route, component})
  }

  resolve(url: string): JSX.Element {
    for(let r of this.routes) {
      let identifierRegExp = /\{([^\}]+)\}/gi
      let routeRegExp = new RegExp('^' + r.route.replace(identifierRegExp, '([^/]+)'), "gi")
      const routeMatches = routeRegExp.exec(url)
      if(!routeMatches) {
        continue;
      }

      let identifierValues: {[key: string]: string} = {}
      let identifierMatches: any = null;
      
      for(let i = 1; i < routeMatches.length; i++) {
        identifierMatches = identifierRegExp.exec(r.route)
        identifierValues[identifierMatches[1]] = routeMatches[i]
      }
      
      return r.component(identifierValues)
    }
    return React.createElement('div', null, 'Page not found')
  }
}
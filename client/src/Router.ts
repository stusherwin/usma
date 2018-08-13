import * as React from 'react';

export class Router {
  private routes: {route: string, component: (c: {[key: string]: number}) => JSX.Element}[] = []
  
  route(route: string, component: (c: {[key: string]: number}) => JSX.Element) {
    this.routes.push({route, component})
  }

  resolve(url: string): JSX.Element {
    let identifierParseFail = false
    for(let r of this.routes) {
      const identifierRegExp = /\{([^\}]+)\}/gi
      const routeRegExp = new RegExp('^' + r.route.replace(identifierRegExp, '([^/]+)'), "gi")
      const routeMatches = routeRegExp.exec(url)
      if(!routeMatches) {
        continue;
      }

      const identifierValues: {[key: string]: number} = {}
      let identifierMatches: any = null;
      
      for(let i = 1; i < routeMatches.length; i++) {
        identifierMatches = identifierRegExp.exec(r.route)
        const value = parseInt(routeMatches[i])

        if(isNaN(value)) {
          identifierParseFail = true
          break
        }
        identifierValues[identifierMatches[1]] = value
      }

      if(identifierParseFail) {
        break
      }
      
      return r.component(identifierValues)
    }
    
    return React.createElement('div', null, 'Page not found')
  }
}
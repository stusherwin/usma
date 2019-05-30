import * as React from 'react';

export class Router {
  private basePath: string
  static path: string
  private routes: {route: string, component: (c: {[key: string]: number}, r: Router) => JSX.Element | undefined}[] = []

  constructor(basePath: string) {
    this.basePath = basePath
  }

  route(route: string, component: (c: {[key: string]: number}, r: Router) => JSX.Element | undefined) {
    let r = this.routes.find(r => r.route == route)

    if(r) {
      r.component = component
    } else {
      this.routes.push({route, component})
    }
  }

  resolve(): JSX.Element | undefined {
    let identifierParseFail = false
    const identifierRegExp = /\{([^\}]+)\}/gi
    const baseRouteRegExp = new RegExp('^' + this.basePath.replace(identifierRegExp, '([^/]+)'), "gi")
    const url = Router.path.replace(baseRouteRegExp, '')

    for(let r of this.routes) {
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
      
      let component = r.component(identifierValues, new Router(r.route))
      if(component) {
        return component
      } else {
        return React.createElement('div', { className: "p-2" }, 'Page not found')
      }
    }

    console.log(this.basePath + ': Page not found for url: ' + url)
    return React.createElement('div', { className: "p-2" }, 'Page not found')
  }

  static navigate(url: string) {
    window.history.pushState(url, url, url);
  }

  static updatePath(path: string) {
    this.path = Router.normalise(path)
  }

  static isCurrent(path: string) {
    return this.path == Router.normalise(path)
  }

  private static normalise(url: string) {
    return url.endsWith('/') ? url : url + '/';
  }
}
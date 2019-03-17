import * as React from 'react';

export class Router {
  private baseUrl: string
  static url: string
  private routes: {route: string, component: (c: {[key: string]: number}, r: Router) => JSX.Element | undefined}[] = []

  constructor(baseUrl: string) {
    this.baseUrl = baseUrl
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
    const baseRouteRegExp = new RegExp('^' + this.baseUrl.replace(identifierRegExp, '([^/]+)'), "gi")
    const url = Router.url.replace(baseRouteRegExp, '')

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
      }
    }

    console.log(this.baseUrl + ': Page not found for url: ' + url)
    return React.createElement('div', null, 'Page not found')
  }

  static navigate(url: string) {
    window.history.pushState(url, url, url);
  }

  static updateUrl(url: string) {
    this.url = url.endsWith('/') ? url : url + '/'
  }
}
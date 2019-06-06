import * as React from 'react';
import * as classNames from 'classnames'

import { Icon } from '../common/Icon'

const transitionTime = 0.25;
const minHeight = '5rem';

export interface CollapsibleProps { className?: string 
                                  , expanded: boolean
                                  , otherExpanding: boolean
                                  , visible?: boolean
                                  , header: () => JSX.Element
                                  , toggle: () => void
                                  , onExpand?: () => void
                                  , onCollapse?: () => void
                                  , onExpanded?: () => void
                                  , onCollapsed?: () => void
                                  }

export class Collapsible extends React.Component<CollapsibleProps, {}> {
  container: React.RefObject<HTMLDivElement>

  constructor(props: CollapsibleProps) {
    super(props)

    this.container = React.createRef();
  }

  componentDidUpdate(prevProps: CollapsibleProps) {
    if(prevProps.expanded != this.props.expanded) {
      this.animateHeight()
      if(this.props.expanded) {
        if(this.props.onExpand) {
          this.props.onExpand()
        }
      } else {
        if(this.props.onCollapse) {
          this.props.onCollapse()
        }
      }
    }
  }

  componentDidMount() {
    this.animateHeight()
  }

  animateHeight() {
    const el = this.container.current
    if(!el) return

    if(this.props.expanded) {
      el.style.height = el.scrollHeight + 'px';
    } else {
      el.style.height = el.scrollHeight + 'px';
      el.offsetHeight; // trigger reflow
      el.style.height = minHeight;
    }
  }

  transitionEnded = () => {
    const el = this.container.current
    if(el) {
      if(this.props.expanded) {
        el.style.height = null;
      }
    }

    if(this.props.expanded) {
      if(this.props.onExpanded) {
        this.props.onExpanded();
      }
    } else {
      if(this.props.onCollapsed) {
        this.props.onCollapsed();
      }
    }
  }

  visible() {
    if(this.props.visible == undefined) return true

    return this.props.visible
  }
  
  render() {
    return (
      <div ref={this.container} className={classNames('relative overflow-hidden', this.props.className)} style={{ 
            height: minHeight,
            transition: `height ${transitionTime / 2}s ease`,
            transitionDelay: this.props.expanded? '0s' : (this.props.otherExpanding? `${transitionTime / 2}s` : '0s')
          }} onTransitionEnd={this.transitionEnded}>
        <a href="#" onClick={e => { e.preventDefault(); e.stopPropagation(); this.props.toggle() }}
           className="block no-underline text-black hover:text-black hover:no-underline">
           {this.visible() && 
             <Icon type={this.props.expanded? 'collapse' : 'expand'} className="w-4 h-4 fill-current absolute pin-r mt-3 mr-2" />
           }
          { this.props.header() }
        </a>
        { this.props.children }
      </div>
    )
  }
}

export interface HeaderProps { headerClassName?: string 
                             , headingClassName?: string 
                             , headerImageClassName?: string
                             , headerText?: string
                             , headerContent?: () => JSX.Element
                             , visible?: boolean
                             }

export class Header extends React.Component<HeaderProps, {}> {
  visible() {
    if(this.props.visible == undefined) return true

    return this.props.visible
  }

  render() {
    return (
      <div className={classNames('p-2', this.props.headerClassName)}>
        {this.visible() &&
          <div className={classNames('bg-no-repeat w-16 h-16 absolute', this.props.headerImageClassName)}></div>
        }
        <h2 className={classNames('leading-none ml-20 relative flex', this.props.headingClassName)}>
          {this.props.headerText}
        </h2>
        { this.props.headerContent && this.props.headerContent() }
      </div>
    )
  }
}

export interface SmallHeaderProps { headerClassName?: string 
                                  , headingClassName?: string 
                                  , headerImageClassName?: string
                                  , headerText?: string
                                  , headerContent?: () => JSX.Element
                                  , visible?: boolean
                                  }

export class SmallHeader extends React.Component<SmallHeaderProps, {}> {
  visible() {
    if(this.props.visible == undefined) return true

    return this.props.visible
  }

  render() {
    return (
      <div className={classNames('p-2', this.props.headerClassName)}>
        {this.visible() &&
          <div className={classNames('bg-no-repeat w-16 h-16 absolute', this.props.headerImageClassName)}></div>
        }
        <h3 className={classNames('leading-none ml-20 relative flex', this.props.headingClassName)}>
          {this.props.headerText}
        </h3>
        { this.props.headerContent && this.props.headerContent() }
      </div>
    )
  }
}
import * as React from 'react';
import * as classNames from 'classnames'

import { Icon } from './Icon'

const transitionTime = 0.25;

export class CollapsibleState {
  expanded: string | number | null
  update: (state: CollapsibleState) => void

  constructor(expanded: string | number | null, update: (state: CollapsibleState) => void) {
    this.expanded = expanded
    this.update = update
  }

  isExpanded(key: string | number) {
    return this.expanded == key
  }

  otherExpanding(key: string | number) {
    return !!this.expanded && this.expanded != key
  }
  
  toggle = (toExpand: string | number) => () => { 
    this.update(new CollapsibleState(toExpand == this.expanded? null : toExpand, this.update))
  }
}

export interface CollapsibleProps { className?: string 
                                  , minHeight?: number
                                  , onExpand?: () => void
                                  , onCollapse?: () => void
                                  , onExpanded?: () => void
                                  , onCollapsed?: () => void
                                  , header: JSX.Element
                                  , expandedHeader?: JSX.Element
                                  , collapsibleKey: string | number
                                  , collapsibleState  : CollapsibleState
                                  }

export class Collapsible extends React.Component<CollapsibleProps, {}> {
  container: React.RefObject<HTMLDivElement>
  minHeight: string;

  constructor(props: CollapsibleProps) {
    super(props)

    this.container = React.createRef();
    this.minHeight = (24 / 4) + 'rem';
  }

  componentDidUpdate(prevProps: CollapsibleProps) {
    let prevExpanded = prevProps.collapsibleState.isExpanded(this.props.collapsibleKey)
    let expanded = this.props.collapsibleState.isExpanded(this.props.collapsibleKey)

    if(prevExpanded != expanded) {
      this.animateHeight()
      if(expanded) {
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

    if(this.props.collapsibleState.isExpanded(this.props.collapsibleKey)) {
      el.style.height = el.scrollHeight + 'px';
    } else {
      el.style.height = el.scrollHeight + 'px';
      el.offsetHeight; // trigger reflow
      el.style.height = this.minHeight;
    }
  }

  transitionEnded = () => {
    const el = this.container.current
    if(el) {
      if(this.props.collapsibleState.isExpanded(this.props.collapsibleKey)) {
        el.style.height = null;
      }
    }

    if(this.props.collapsibleState.isExpanded(this.props.collapsibleKey)) {
      if(this.props.onExpanded) {
        this.props.onExpanded();
      }
    } else {
      if(this.props.onCollapsed) {
        this.props.onCollapsed();
      }
    }
  }

  render() {
    return (
      <div ref={this.container} className={classNames('relative overflow-hidden', this.props.className)} style={{ 
            height: this.minHeight,
            transition: `height ${transitionTime / 2}s ease`,
            transitionDelay: this.props.collapsibleState.isExpanded(this.props.collapsibleKey)? '0s' : (this.props.collapsibleState.otherExpanding(this.props.collapsibleKey)? `${transitionTime / 2}s` : '0s')
          }} onTransitionEnd={this.transitionEnded}>
        <a href="#" onClick={e => { e.preventDefault(); e.stopPropagation(); this.props.collapsibleState.toggle(this.props.collapsibleKey)() }} className="block no-underline text-black hover:text-black hover:no-underline relative">
           <Icon type={this.props.collapsibleState.isExpanded(this.props.collapsibleKey)? 'collapse' : 'expand'} className="w-4 h-4 fill-current absolute pin-r pin-b mb-4 mr-2" />
          { this.props.header }
        </a>
        { this.props.expandedHeader }
        { this.props.children }
      </div>
    )
  }
}
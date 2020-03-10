import * as React from 'react';

import { Icon } from './Icon'

const transitionTime = 0.125;

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
    console.log('click');
    this.update(new CollapsibleState(toExpand == this.expanded? null : toExpand, this.update))
  }
}

export interface CollapsibleProps { onExpand?: () => void
                                  , onCollapse?: () => void
                                  , onExpanded?: () => void
                                  , onCollapsed?: () => void
                                  , header: (ref: React.RefObject<HTMLDivElement>) => JSX.Element
                                  , expandedHeader?: JSX.Element
                                  , collapsibleKey: string | number
                                  , collapsibleState  : CollapsibleState
                                  }

interface State { minHeight: string }

export class Collapsible extends React.Component<CollapsibleProps, State> {
  container: React.RefObject<HTMLDivElement>
  header: React.RefObject<HTMLDivElement>

  constructor(props: CollapsibleProps) {
    super(props)

    this.container = React.createRef();
    this.header = React.createRef();
    this.state = { minHeight: (24 / 4) + 'rem' }
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
    this.setState({minHeight: !!this.header.current && (this.header.current.clientHeight + 'px') || ((24 / 4) + 'rem')});

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
      el.style.height = this.state.minHeight;
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
      <div ref={this.container} className="relative overflow-hidden" style={{ 
            height: this.props.collapsibleState.isExpanded(this.props.collapsibleKey) ? undefined : this.state.minHeight,
            transition: `height ${transitionTime / 2}s ease`,
            transitionDelay: this.props.collapsibleState.isExpanded(this.props.collapsibleKey)? '0s' : (this.props.collapsibleState.otherExpanding(this.props.collapsibleKey)? `${transitionTime / 2}s` : '0s')
          }} onTransitionEnd={this.transitionEnded}>
        <a href="#" onClick={e => { e.preventDefault(); e.stopPropagation(); this.props.collapsibleState.toggle(this.props.collapsibleKey)() }} className="block no-underline text-black hover:text-black hover:no-underline relative">
           <Icon type={this.props.collapsibleState.isExpanded(this.props.collapsibleKey)? 'collapse' : 'expand'} className="w-4 h-4 fill-current absolute pin-r pin-b mb-4 mr-2" />
          { this.props.header(this.header) }
        </a>
        { this.props.expandedHeader }
        { this.props.children }
      </div>
    )
  }
}
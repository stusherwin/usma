import * as React from 'react';
import * as classNames from 'classnames'

import { Icon } from '../common/Icon'

const transitionTime = 0.25;
const minHeight = '5rem';

export interface CollapsibleWithHeaderProps { className: string 
                                            , headerClassName: string 
                                            , headerImageClassName: string
                                            , headerText: string
                                            , headerContent: () => JSX.Element
                                            , expanded: boolean
                                            , otherExpanding: boolean
                                            , toggle: () => void
                                            , onExpand?: () => void
                                            , onCollapse?: () => void
                                            }

export class CollapsibleWithHeader extends React.Component<CollapsibleWithHeaderProps, {}> {
  container: React.RefObject<HTMLDivElement>

  constructor(props: CollapsibleWithHeaderProps) {
    super(props)

    this.container = React.createRef();
  }

  componentDidUpdate(prevProps: CollapsibleWithHeaderProps) {
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

  unsetHeight = () => {
    const el = this.container.current
    if(!el) return

    if(this.props.expanded) {
      el.style.height = null;
    }
  }
  
  render() {
    return (
      <div ref={this.container} className={this.props.className} style={{ 
            overflow: 'hidden',
            height: minHeight,
            transition: `height ${transitionTime / 2}s ease`,
            transitionDelay: this.props.expanded? '0s' : (this.props.otherExpanding? `${transitionTime / 2}s` : '0s')
          }} onTransitionEnd={this.unsetHeight}>
        <a href="#" onClick={e => { e.preventDefault(); this.props.toggle() }} className={classNames(
            'p-2 block no-underline text-black hover:text-black hover:no-underline',
            this.props.headerClassName)}>
          <div className={classNames('bg-no-repeat w-16 h-16 absolute', this.props.headerImageClassName)}></div>
          <h2 className="leading-none ml-20 relative flex">{this.props.headerText}
            <Icon type={this.props.expanded? 'collapse' : 'expand'} className="w-4 h-4 fill-current absolute pin-r mt-1" />
          </h2>
          { this.props.headerContent() }
        </a>
        <div className="shadow-inner-top bg-white px-2 py-4">
          { this.props.children }
        </div>
      </div>
    )
  }
}
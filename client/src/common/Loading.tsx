import * as React from 'react';
import * as classNames from 'classnames'

import { Icon } from './Icon'

export interface LoadingProps {
  loading: boolean
}

export interface LoadingState {
  loading: boolean
}

export class Loading extends React.Component<LoadingProps, LoadingState> {
  constructor(props: LoadingProps) {
    super(props)

    this.state = { loading: false }
  }

  loading = () => this.props.loading || this.state.loading

  componentDidUpdate(prevProps: LoadingProps) {
    if(!prevProps.loading && this.props.loading) {
      this.setState({ loading: true })
    }
  }

  transitionEnded = () => {
    if(!this.props.loading) {
      this.setState({ loading: false })
    }
  }

  render() {
    return (
      <div className={classNames('fixed pin bg-black flex items-center justify-center text-grey-lighter', {
          'pointer-events-none': !this.loading()
        })} style={{
          background: 'rgba(0,0,0,0.3)',
          opacity: this.props.loading? 1 : 0,
          transition: 'opacity 0.25s ease'
        }} onTransitionEnd={this.transitionEnded}>
        <Icon type="loading" className={classNames("w-16 h-16 -mt-4 fill-current", {"rotating": this.loading()})} />
      </div>
    )
  }
}
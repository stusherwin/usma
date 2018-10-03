import * as React from 'react';

import { Icon } from './Icon'

const loadMoretriggerMargin = 50

export interface LoadMoreProps { loadMore: () => void
                               }

export interface LoadMoreState { loadMoreVisible: boolean
                               }

export class LoadMore extends React.Component<LoadMoreProps, LoadMoreState> {
  constructor(props: LoadMoreProps) {
    super(props)

    this.state = { loadMoreVisible: false
                 }
  }

  componentDidMount() {
    const checkLoadMoreTriggered = () => {
      const loadMore = document.getElementById('load-more')
      if(!loadMore) return false
      
      const rect = loadMore.getBoundingClientRect()
      const loadMoreVisible = rect.top - window.innerHeight < loadMoretriggerMargin
      if(loadMoreVisible !== this.state.loadMoreVisible) {
        this.setState({loadMoreVisible})
        if(loadMoreVisible) {
          this.props.loadMore()
        }
      }

      return false
    }
    
    checkLoadMoreTriggered()
    document.onscroll = checkLoadMoreTriggered
  }

  render() {
    return (
      <div id="load-more" className="bg-grey-lightest py-8 text-center text-grey">
        <Icon type="loading" className="w-4 h-4 mx-auto rotating ml-2 fill-current" />
      </div>
    )
  }
}
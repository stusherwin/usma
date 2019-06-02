import * as React from 'react';
import * as classNames from 'classnames'

import { Icon } from '../common/Icon'

export interface LoadingProps {
  loading: boolean
}

export const Loading = ({loading}: LoadingProps) => 
  <div className={classNames('fixed pin bg-black flex items-center justify-center text-grey-lighter', {
      'pointer-events-none': !loading
    })} style={{
      background: 'rgba(0,0,0,0.3)',
      opacity: loading? 1 : 0,
      transition: 'opacity 0.25s ease'
    }}>
    <Icon type="loading" className="w-16 h-16 -mt-4 rotating fill-current" />
  </div>

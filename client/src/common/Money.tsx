import * as React from 'react';

import { Util } from './Util'

export const Money = (props: {amount: number, absolute?: boolean, className?: string}) => {
  const absolute = props.absolute || false
  const className = (!props.absolute && props.amount < 0 ? 'text-red ' : '') + props.className

  return <span className={className}>{!absolute && props.amount < 0 && '-'}&pound;{ Util.formatMoney(props.amount, true) }</span>
}
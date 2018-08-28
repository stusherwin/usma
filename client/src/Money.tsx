import * as React from 'react';

import { Util } from './Util'

export const Money = (props: {amount: number, absolute?: boolean}) => {
  const absolute = props.absolute || false

  return <span>{!absolute && props.amount < 0 && '-'} &pound;{ Util.formatMoney(props.amount, true) }</span>
}
import * as React from 'react';
import * as classNames from 'classnames'

import { Util } from './Util'

export interface MoneyProps {
  amount: number
  absolute?: boolean
  noColour?: boolean
  className?: string
}

export const Money = ({amount, absolute, noColour, className}: MoneyProps) => {
  absolute = absolute || false
  className = (!absolute && !noColour && amount < 0 ? 'text-red ' : '') + className
  
  return <span className={className}>{!absolute && amount < 0 && '-'}&pound;{ Util.formatMoney(amount, true)}</span>
}

export interface BalanceProps {
  amount: number
  className?: string
}

export const Balance = ({amount, className}: BalanceProps) => 
  <h3 className={classNames("p-1 rounded-sm w-20 text-center", className)}>
    <div className={classNames("", {'text-green-dark': amount <= 0, 'text-red': amount > 0 })}>&pound;{ Util.formatMoney(amount, true)}</div>
    {amount <= 0? 
      <div className="uppercase text-xs text-green-dark">In credit</div>
    : <div className="uppercase text-xs text-red">To pay</div>
    }
  </h3>
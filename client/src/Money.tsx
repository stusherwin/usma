import * as React from 'react';

import { Util } from './Util'

export class Money extends React.Component<{amount: number}, {}> {
  render() {
    return <span>{this.props.amount < 0 && '-'}&pound;{ Util.formatMoney(this.props.amount) }</span>
  }
}
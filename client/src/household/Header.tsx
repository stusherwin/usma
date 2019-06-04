import * as React from 'react';
import * as classNames from 'classnames'

export interface HeaderProps { className?: string 
                             , headingClassName?: string 
                             , imageClassName?: string
                             , headingText?: string
                             , content?: () => JSX.Element
                             }

export class Header extends React.Component<HeaderProps, {}> {
  render() {
    return (
      <div className={classNames(
          'p-2 text-black',
          this.props.className)}>
        <div className={classNames('bg-no-repeat w-16 h-16 absolute', this.props.imageClassName)}></div>
        <h2 className={classNames('leading-none ml-20 relative flex', this.props.headingClassName)}>{this.props.headingText}</h2>
        { this.props.content && this.props.content() }
      </div>
    )
  }
}
import * as React from 'react';
import * as classNames from 'classnames'
import { Field } from './Validation'

export interface TextFieldProps {
  id: string
  label: string
  field: Field
  valueOnChange: (e: React.ChangeEvent<HTMLInputElement>) => void
}

export const TextField = (props: TextFieldProps) => (
  <div className={classNames('field mb-4', {'invalid': !props.field.valid})}>
    <div className="flex justify-between items-baseline">
      <label className="flex-no-grow flex-no-shrink mr-2"
             htmlFor={props.id}>{props.label}:</label>
      <input type="text" 
             id={props.id} 
             autoFocus
             className="flex-grow flex-no-shrink"
             value={props.field.stringValue} 
             onChange={props.valueOnChange} />
    </div>
    <div className="text-red mt-2" hidden={!props.field.error}>
      {props.field.error}
    </div>
  </div>
)

export interface MoneyFieldProps {
  id: string
  label: string
  field: Field
  valueOnChange: (e: React.ChangeEvent<HTMLInputElement>) => void
}

export const MoneyField = (props: MoneyFieldProps) => (
  <div className={classNames('field mb-4', {'invalid': !props.field.valid})}>
    <div className="flex justify-between items-baseline">
      <label className="flex-no-grow flex-no-shrink mr-1"
             htmlFor={props.id}>{props.label}:<span className="ml-2">&pound;</span></label>
      <input type="text"
             id={props.id}
             className="flex-grow flex-no-shrink"
             value={props.field.stringValue}
             onChange={props.valueOnChange} />
    </div>
    <div className="text-red mt-2" hidden={!props.field.error}>
      {props.field.error}
    </div>
  </div>
)
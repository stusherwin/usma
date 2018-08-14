export class Validate {
  validate: (stringValue: string) => boolean
  error: string

  constructor(validate: (stringValue: string) => boolean, error: string) {
    this.validate = validate
    this.error = error
  }

  static required(error: string) {
    return new Validate((v: string) => !!v.length, error)
  }

  static decimal(error: string) {
    return new Validate((v: string) => /^\-?\d+(:?\.\d+)?$/.test(v), error)
  }

  static twoDP(error: string) {
    return new Validate((v: string) => /^\-?\d+(:?\.\d{0,2})?$/.test(v), error)
  }

  static greaterThanZero(error: string) {
    return new Validate((v: string) => parseFloat(v) > 0, error)
  }
}

export class Field {
  private defaultValue: any
  stringValue: string
  value: any
  valid: boolean
  error: string | null
  private validation: Validate[]
  private parse: (stringValue: string) => any

  private constructor(defaultValue: any, parse: (stringValue: string) => any, validation: Validate[], stringValue: string = '', value: any = defaultValue, valid: boolean = true, error: string | null = null) {
    this.defaultValue = defaultValue
    this.parse = parse
    this.validation = validation
    this.stringValue = stringValue
    this.value = value
    this.valid = valid
    this.error = error
  }

  static create(defaultValue: any, parse: (stringValue: string) => any, validation: Validate[]): Field {
    return new Field(defaultValue, parse, validation)
  }

  clone(): Field {
    return new Field(this.defaultValue, this.parse, this.validation, this.stringValue, this.value, this.valid, this.error)
  }

  reset(): Field {
    let field = this.clone()

    field.stringValue = ''
    field.value = field.defaultValue
    field.valid = true
    field.error = null

    return field
  }

  validate(): Field {
    let field = this.clone()

    field.error = null
    field.valid = true
    for(let v of field.validation) {
      if(!v.validate(field.stringValue)) {
        field.error = v.error
        field.valid = false
        break
      }
    }

    return field
  }

  update(stringValue: string): Field {
    let field = this.clone()

    field.stringValue = stringValue
    field.value = field.parse(stringValue)

    return field
  }
}

export class Form {
  fields: {[key: string]: Field}
  private validating: boolean
  private allFieldsValid: boolean

  private constructor(fields: {[key: string]: Field}, validating: boolean = false, allFieldsValid: boolean = true) {
    this.fields = fields
    this.validating = validating
    this.allFieldsValid = allFieldsValid
  }

  static create(fields: {[key: string]: Field}): Form {
    return new Form(fields)
  }

  clone(): Form {
    const fields: {[key: string]: Field} = {}

    for(let f in this.fields) {
      fields[f] = this.fields[f].clone()
    }

    return new Form(fields, this.validating, this.allFieldsValid)
  }

  reset(): Form {
    const fields: {[key: string]: Field} = {}

    for(let f in this.fields) {
      fields[f] = this.fields[f].reset()
    }

    return new Form(fields, false, true)
  }

  validate(): Form {
    const fields: {[key: string]: Field} = {}
    let allFieldsValid = true

    for(let f in this.fields) {
      const field = this.fields[f].validate()
      fields[f] = field
      allFieldsValid = allFieldsValid && field.valid
    }

    return new Form(fields, true, allFieldsValid)
  }

  update(fieldName: string, stringValue: string): Form {
    const fields: {[key: string]: Field} = {}

    for(let f in this.fields) {
      fields[f] = f == fieldName ? this.fields[f].update(stringValue) : this.fields[f].clone()
    }

    const form = new Form(fields, this.validating, this.allFieldsValid)
    return this.validating ? form.validate() : form
  }

  valid(): boolean {
    return !this.validating || this.allFieldsValid
  }
}
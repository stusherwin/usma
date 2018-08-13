const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
export class Util {
  static formatDate(date: Date): string {
    return `${date.getUTCDate()} ${months[date.getUTCMonth()]} ${date.getUTCFullYear()}`
  }

  static dateString(date: Date): string {
    const y = date.getUTCFullYear()
    const m = date.getUTCMonth() + 1
    const d = date.getUTCDate()
    return `${y}-${(m < 10 ? '0' : '') + m}-${(d < 10 ? '0' : '') + d}`
  }

  static formatMoney(pence: number): string {
    return (pence / 100.0).toFixed(2)
  }
}
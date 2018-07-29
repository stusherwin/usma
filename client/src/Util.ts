const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
export class Util {
  static formatDate(date: Date): string {
    return `${date.getUTCDate()} ${months[date.getUTCMonth()]} ${date.getUTCFullYear()}`
  }

  static formatMoney(pence: number): string {
    return (pence / 100.0).toFixed(2)
  }
}
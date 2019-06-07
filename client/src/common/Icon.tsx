import * as React from 'react';

const icons: {[key: string]: (className?: string) => JSX.Element} = 
  { 'edit': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="433" height="433" viewBox="0 0 432.5 432.5"><path d="M0 313.8v118.8h118.8l237.5-237.5L237.5 76.2 0 313.8zM103.6 396L103.6 396l-30.5 0v-36.5H36.5v-30.6l26-26 67.1 67.1L103.6 396zM246.7 124.8c4.2 0 6.3 2.1 6.3 6.3 0 1.9-0.7 3.5-2 4.9L96.2 290.7c-1.3 1.3-3 2-4.9 2 -4.2 0-6.3-2.1-6.3-6.3 0-1.9 0.7-3.5 2-4.9l154.7-154.7C243.2 125.4 244.8 124.8 246.7 124.8z"/><path d="M422 77.7l-67.1-66.8C347.7 3.6 339 0 328.9 0c-10.3 0-18.8 3.6-25.7 10.8l-47.4 47.1 118.8 118.8 47.4-47.4c7-7 10.6-15.6 10.6-25.7C432.5 93.7 429 85.1 422 77.7z"/></svg>
  , 'delete': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="439" height="439" viewBox="0 0 438.5 438.5"><path d="M417.7 75.7c-1.7-1.7-3.9-2.6-6.6-2.6h-88.2L302.9 25.4c-2.9-7-8-13-15.4-18C280.1 2.5 272.6 0 264.9 0h-91.4c-7.6 0-15.1 2.5-22.6 7.4 -7.4 4.9-12.6 10.9-15.4 18l-20 47.7h-88.2c-2.7 0-4.9 0.9-6.6 2.6 -1.7 1.7-2.6 3.9-2.6 6.6v18.3c0 2.7 0.9 4.9 2.6 6.6 1.7 1.7 3.9 2.6 6.6 2.6h27.4v271.8c0 15.8 4.5 29.3 13.4 40.4 8.9 11.1 19.7 16.7 32.3 16.7h237.5c12.6 0 23.3-5.8 32.3-17.3 8.9-11.5 13.4-25.2 13.4-41V109.6h27.4c2.7 0 4.9-0.9 6.6-2.6 1.7-1.7 2.6-3.9 2.6-6.6V82.2C420.3 79.6 419.4 77.4 417.7 75.7zM169.3 39.7c1.3-1.7 3-2.8 4.9-3.1h90.5c1.9 0.4 3.5 1.4 4.9 3.1l13.7 33.4H155.3L169.3 39.7zM347.2 380.3c0 4.2-0.7 8-2 11.6 -1.3 3.5-2.7 6.1-4.1 7.7 -1.4 1.6-2.4 2.4-3 2.4H100.5c-0.6 0-1.6-0.8-3-2.4 -1.4-1.6-2.8-4.2-4.1-7.7 -1.3-3.5-2-7.4-2-11.6V109.6h255.8V380.3z"/><path d="M137 347.2h18.3c2.7 0 4.9-0.9 6.6-2.6 1.7-1.7 2.6-3.9 2.6-6.6V173.6c0-2.7-0.9-4.9-2.6-6.6 -1.7-1.7-3.9-2.6-6.6-2.6H137c-2.7 0-4.9 0.9-6.6 2.6 -1.7 1.7-2.6 3.9-2.6 6.6v164.5c0 2.7 0.9 4.9 2.6 6.6C132.2 346.3 134.4 347.2 137 347.2z"/><path d="M210.1 347.2h18.3c2.7 0 4.9-0.9 6.6-2.6 1.7-1.7 2.6-3.9 2.6-6.6V173.6c0-2.7-0.9-4.9-2.6-6.6 -1.7-1.7-3.9-2.6-6.6-2.6h-18.3c-2.7 0-4.9 0.9-6.6 2.6 -1.7 1.7-2.6 3.9-2.6 6.6v164.5c0 2.7 0.9 4.9 2.6 6.6C205.3 346.3 207.5 347.2 210.1 347.2z"/><path d="M283.2 347.2h18.3c2.7 0 4.9-0.9 6.6-2.6 1.7-1.7 2.6-3.9 2.6-6.6V173.6c0-2.7-0.9-4.9-2.6-6.6 -1.7-1.7-3.9-2.6-6.6-2.6H283.2c-2.7 0-4.9 0.9-6.6 2.6 -1.7 1.7-2.6 3.9-2.6 6.6v164.5c0 2.7 0.9 4.9 2.6 6.6C278.4 346.3 280.6 347.2 283.2 347.2z"/></svg>
  , 'refresh': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="439" height="439" viewBox="0 0 438.5 438.5"><path d="M427.4 19.7c-7.8-3.2-14.5-1.9-20 4l-37.1 36.8C349.9 41.3 326.7 26.4 300.5 15.8 274.3 5.3 247.3 0 219.3 0c-29.7 0-58.1 5.8-85.1 17.4 -27 11.6-50.3 27.2-70 46.8 -19.6 19.6-35.2 42.9-46.8 69.9C5.8 161.2 0 189.6 0 219.3c0 29.7 5.8 58.1 17.4 85.1 11.6 27 27.2 50.3 46.8 70 19.6 19.6 42.9 35.2 70 46.8 27 11.6 55.4 17.4 85.1 17.4 32.7 0 63.9-6.9 93.4-20.7 29.5-13.8 54.6-33.3 75.4-58.4 1.5-1.9 2.2-4 2.1-6.4 -0.1-2.4-1-4.3-2.7-5.9l-39.1-39.4c-2.1-1.7-4.5-2.6-7.1-2.6 -3 0.4-5.2 1.5-6.6 3.4 -13.9 18.1-30.9 32.1-51.1 42 -20.2 9.9-41.6 14.8-64.2 14.8 -19.8 0-38.7-3.9-56.7-11.6 -18-7.7-33.6-18.1-46.7-31.3 -13.1-13.1-23.6-28.7-31.3-46.7 -7.7-18-11.6-36.9-11.6-56.7 0-19.8 3.9-38.7 11.6-56.7 7.7-18 18.1-33.5 31.3-46.7 13.1-13.1 28.7-23.6 46.7-31.3 18-7.7 36.9-11.6 56.7-11.6 38.3 0 71.5 13 99.6 39.1l-39.4 39.4c-5.9 5.7-7.2 12.3-4 19.7 3.2 7.6 8.9 11.4 16.9 11.4h127.9c4.9 0 9.2-1.8 12.8-5.4 3.6-3.6 5.4-7.9 5.4-12.8V36.6C438.5 28.6 434.8 22.9 427.4 19.7z"/></svg>
  , 'question': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="439" height="439" viewBox="0 0 438.5 438.5"><path d="M409.1 109.2c-19.6-33.6-46.2-60.2-79.8-79.8C295.7 9.8 259.1 0 219.3 0c-39.8 0-76.5 9.8-110.1 29.4 -33.6 19.6-60.2 46.2-79.8 79.8C9.8 142.8 0 179.5 0 219.3c0 39.8 9.8 76.5 29.4 110.1 19.6 33.6 46.2 60.2 79.8 79.8 33.6 19.6 70.3 29.4 110.1 29.4s76.5-9.8 110.1-29.4c33.6-19.6 60.2-46.2 79.8-79.8 19.6-33.6 29.4-70.3 29.4-110.1C438.5 179.5 428.7 142.8 409.1 109.2zM255.8 356.3c0 2.7-0.9 4.9-2.6 6.6 -1.7 1.7-3.9 2.6-6.6 2.6h-54.8c-2.7 0-4.9-0.9-6.6-2.6 -1.7-1.7-2.6-3.9-2.6-6.6v-54.8c0-2.7 0.9-4.9 2.6-6.6 1.7-1.7 3.9-2.6 6.6-2.6h54.8c2.7 0 4.9 0.9 6.6 2.6 1.7 1.7 2.6 3.9 2.6 6.6V356.3zM325.3 187.6c-2.4 7-5 12.8-8 17.3 -2.9 4.5-7.2 9-12.7 13.7 -5.5 4.7-9.9 8-13.1 10 -3.2 2-7.9 4.7-14 8.1 -6.3 3.6-11.5 8.3-15.6 14.1 -4.1 5.8-6.1 10.5-6.1 14.1 0 2.7-0.9 4.9-2.6 6.6 -1.7 1.7-3.9 2.6-6.6 2.6h-54.8c-2.7 0-4.9-0.9-6.6-2.6 -1.7-1.7-2.6-3.9-2.6-6.6v-10.3c0-12.8 5-24.7 15-35.8 10-11.1 21-19.4 33-24.7 9.1-4.2 15.6-8.5 19.4-12.8 3.8-4.4 5.7-10.2 5.7-17.4 0-6.3-3.6-11.9-10.7-16.8 -7.1-4.9-15.3-7.4-24.4-7.4 -9.9 0-18.1 2.3-24.6 6.9 -6.3 4.6-14.5 13.3-24.6 26.3 -1.7 2.3-4.1 3.4-7.1 3.4 -2.3 0-4.1-0.6-5.4-1.7L121.4 145.9c-4.4-3.4-5.1-7.4-2.3-12 24.4-40.5 59.7-60.8 105.9-60.8 16.6 0 32.7 3.9 48.5 11.7 15.8 7.8 29 18.8 39.5 33.1 10.6 14.3 15.8 29.8 15.8 46.5C328.9 172.8 327.7 180.5 325.3 187.6z"/></svg>
  , 'add': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="439" height="439" viewBox="0 0 438.5 438.5"><path d="M409.1 109.2c-19.6-33.6-46.2-60.2-79.8-79.8C295.7 9.8 259.1 0 219.3 0c-39.8 0-76.5 9.8-110.1 29.4 -33.6 19.6-60.2 46.2-79.8 79.8C9.8 142.8 0 179.5 0 219.3c0 39.8 9.8 76.5 29.4 110.1 19.6 33.6 46.2 60.2 79.8 79.8 33.6 19.6 70.3 29.4 110.1 29.4s76.5-9.8 110.1-29.4c33.6-19.6 60.2-46.2 79.8-79.8 19.6-33.6 29.4-70.3 29.4-110.1C438.5 179.5 428.7 142.8 409.1 109.2zM347.2 237.5c0 4.9-1.8 9.2-5.4 12.8 -3.6 3.6-7.9 5.4-12.8 5.4h-73.1v73.1c0 4.9-1.8 9.2-5.4 12.9 -3.6 3.6-7.9 5.4-12.8 5.4h-36.5c-4.9 0-9.2-1.8-12.8-5.4 -3.6-3.6-5.4-7.9-5.4-12.9v-73.1h-73.1c-4.9 0-9.2-1.8-12.8-5.4 -3.6-3.6-5.4-7.9-5.4-12.8v-36.5c0-4.9 1.8-9.2 5.4-12.8 3.6-3.6 7.9-5.4 12.8-5.4h73.1v-73.1c0-4.9 1.8-9.2 5.4-12.8 3.6-3.6 7.9-5.4 12.8-5.4h36.5c4.9 0 9.2 1.8 12.8 5.4 3.6 3.6 5.4 7.9 5.4 12.8v73.1h73.1c4.9 0 9.2 1.8 12.8 5.4 3.6 3.6 5.4 7.9 5.4 12.8V237.5z"/></svg>
  , 'cancel': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="439" height="439" viewBox="0 0 438.5 438.5"><path d="M409.1 109.2c-19.6-33.6-46.2-60.2-79.8-79.8C295.7 9.8 259.1 0 219.3 0c-39.8 0-76.5 9.8-110.1 29.4 -33.6 19.6-60.2 46.2-79.8 79.8C9.8 142.8 0 179.5 0 219.3c0 39.8 9.8 76.5 29.4 110.1 19.6 33.6 46.2 60.2 79.8 79.8 33.6 19.6 70.3 29.4 110.1 29.4s76.5-9.8 110.1-29.4c33.6-19.6 60.2-46.2 79.8-79.8 19.6-33.6 29.4-70.3 29.4-110.1C438.5 179.5 428.7 142.8 409.1 109.2zM322.6 270.9c3.6 3.6 5.4 7.9 5.4 12.9 0 5.1-1.8 9.5-5.4 13.1l-25.7 25.7c-3.6 3.6-8 5.4-13.1 5.4 -4.9 0-9.2-1.8-12.8-5.4l-51.7-51.7 -51.7 51.7c-3.6 3.6-7.9 5.4-12.8 5.4 -5.1 0-9.5-1.8-13.1-5.4l-25.7-25.7c-3.6-3.6-5.4-8-5.4-13.1 0-4.9 1.8-9.2 5.4-12.9l51.7-51.7 -51.7-51.7c-3.6-3.6-5.4-7.9-5.4-12.8 0-5.1 1.8-9.5 5.4-13.1l25.7-25.7c3.6-3.6 8-5.4 13.1-5.4 4.9 0 9.2 1.8 12.8 5.4l51.7 51.7 51.7-51.7c3.6-3.6 7.9-5.4 12.8-5.4 5.1 0 9.5 1.8 13.1 5.4l25.7 25.7c3.6 3.6 5.4 8 5.4 13.1 0 4.9-1.8 9.2-5.4 12.8l-51.7 51.7L322.6 270.9z"/></svg>
  , 'ok': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="439" height="439" viewBox="0 0 438.5 438.5"><path d="M409.1 109.2c-19.6-33.6-46.2-60.2-79.8-79.8C295.7 9.8 259.1 0 219.3 0c-39.8 0-76.5 9.8-110.1 29.4 -33.6 19.6-60.2 46.2-79.8 79.8C9.8 142.8 0 179.5 0 219.3c0 39.8 9.8 76.5 29.4 110.1 19.6 33.6 46.2 60.2 79.8 79.8 33.6 19.6 70.3 29.4 110.1 29.4s76.5-9.8 110.1-29.4c33.6-19.6 60.2-46.2 79.8-79.8 19.6-33.6 29.4-70.3 29.4-110.1C438.5 179.5 428.7 142.8 409.1 109.2zM361.4 185.9L206.4 340.9c-3.6 3.6-8 5.4-13.1 5.4 -4.9 0-9.2-1.8-12.8-5.4L77.1 237.5c-3.4-3.4-5.1-7.7-5.1-12.8 0-5.3 1.7-9.7 5.1-13.1l26-25.7c3.6-3.6 7.9-5.4 12.9-5.4s9.2 1.8 12.9 5.4l64.5 64.5 116.5-116.2c3.6-3.6 7.9-5.4 12.8-5.4 4.9 0 9.2 1.8 12.8 5.4l26 25.7c3.4 3.4 5.1 7.8 5.1 13.1C366.6 178.2 364.9 182.4 361.4 185.9z"/></svg>
  , 'info': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="439" height="439" viewBox="0 0 438.5 438.5"><path d="M409.1 109.2c-19.6-33.6-46.2-60.2-79.8-79.8C295.7 9.8 259.1 0 219.3 0c-39.8 0-76.5 9.8-110.1 29.4 -33.6 19.6-60.2 46.2-79.8 79.8C9.8 142.8 0 179.5 0 219.3c0 39.8 9.8 76.5 29.4 110.1 19.6 33.6 46.2 60.2 79.8 79.8 33.6 19.6 70.3 29.4 110.1 29.4s76.5-9.8 110.1-29.4c33.6-19.6 60.2-46.2 79.8-79.8 19.6-33.6 29.4-70.3 29.4-110.1C438.5 179.5 428.7 142.8 409.1 109.2zM182.7 54.8c0-2.7 0.9-4.9 2.6-6.6 1.7-1.7 3.9-2.6 6.6-2.6h54.8c2.7 0 4.9 0.9 6.6 2.6 1.7 1.7 2.6 3.9 2.6 6.6v45.7c0 2.7-0.9 4.9-2.6 6.6 -1.7 1.7-3.9 2.6-6.6 2.6h-54.8c-2.7 0-4.9-0.9-6.6-2.6 -1.7-1.7-2.6-3.9-2.6-6.6V54.8zM292.4 356.3c0 2.7-0.9 4.9-2.6 6.6 -1.7 1.7-3.9 2.6-6.6 2.6H155.3c-2.7 0-4.9-0.9-6.6-2.6 -1.7-1.7-2.6-3.9-2.6-6.6v-45.7c0-2.7 0.9-4.9 2.6-6.6 1.7-1.7 3.9-2.6 6.6-2.6h27.4v-91.4h-27.4c-2.7 0-4.9-0.9-6.6-2.6 -1.7-1.7-2.6-3.9-2.6-6.6v-45.7c0-2.7 0.9-4.9 2.6-6.6 1.7-1.7 3.9-2.6 6.6-2.6h91.4c2.7 0 4.9 0.9 6.6 2.6 1.7 1.7 2.6 3.9 2.6 6.6v146.2h27.4c2.7 0 4.9 0.9 6.6 2.6 1.7 1.7 2.6 3.9 2.6 6.6v45.7H292.4z"/></svg>
  , 'alert': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="439" height="439" viewBox="0 0 438.5 438.5"><path d="M409.1 109.2c-19.6-33.6-46.2-60.2-79.8-79.8C295.7 9.8 259.1 0 219.3 0c-39.8 0-76.5 9.8-110.1 29.4 -33.6 19.6-60.2 46.2-79.8 79.8C9.8 142.8 0 179.5 0 219.3s9.8 76.5 29.4 110.1c19.6 33.6 46.2 60.2 79.8 79.8 33.6 19.6 70.3 29.4 110.1 29.4s76.5-9.8 110.1-29.4c33.6-19.6 60.2-46.2 79.8-79.8 19.6-33.6 29.4-70.3 29.4-110.1C438.5 179.5 428.7 142.8 409.1 109.2zM255.8 356c0 2.7-0.9 4.9-2.6 6.7s-3.8 2.7-6.3 2.7h-54.8c-2.5 0-4.7-1-6.6-2.9 -1.9-1.9-2.9-4.1-2.9-6.6V301.8c0-2.5 1-4.7 2.9-6.6 1.9-1.9 4.1-2.9 6.6-2.9h54.8c2.5 0 4.6 0.9 6.3 2.7 1.7 1.8 2.6 4 2.6 6.7V356zM255.2 257.8c-0.2 1.9-1.2 3.6-3 5 -1.8 1.4-4 2.1-6.7 2.1h-52.8c-2.7 0-4.9-0.7-6.9-2.1 -1.9-1.4-2.9-3.1-2.9-5L178.2 80.5c0-2.3 1-4 2.9-5.1 1.9-1.5 4.2-2.3 6.9-2.3h62.8c2.7 0 4.9 0.8 6.9 2.3 1.9 1.1 2.8 2.9 2.8 5.1L255.2 257.8z"/></svg>
  , 'remove': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="439" height="439" viewBox="0 0 438.5 438.5"><path d="M409.1 109.2c-19.6-33.6-46.2-60.2-79.8-79.8C295.7 9.8 259.1 0 219.3 0c-39.8 0-76.5 9.8-110.1 29.4 -33.6 19.6-60.2 46.2-79.8 79.8C9.8 142.8 0 179.5 0 219.3c0 39.8 9.8 76.5 29.4 110.1 19.6 33.6 46.2 60.2 79.8 79.8 33.6 19.6 70.3 29.4 110.1 29.4s76.5-9.8 110.1-29.4c33.6-19.6 60.2-46.2 79.8-79.8 19.6-33.6 29.4-70.3 29.4-110.1C438.5 179.5 428.7 142.8 409.1 109.2zM347.2 237.5c0 4.9-1.8 9.2-5.4 12.8 -3.6 3.6-7.9 5.4-12.8 5.4h-219.3c-4.9 0-9.2-1.8-12.8-5.4 -3.6-3.6-5.4-7.9-5.4-12.8v-36.5c0-4.9 1.8-9.2 5.4-12.8 3.6-3.6 7.9-5.4 12.8-5.4h219.3c4.9 0 9.2 1.8 12.8 5.4 3.6 3.6 5.4 7.9 5.4 12.8V237.5z"/></svg>
  , 'undo': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="439" height="439" viewBox="0 0 438.5 438.5"><path d="M421.1 134.2c-11.6-27-27.2-50.3-46.8-69.9C354.7 44.6 331.4 29 304.4 17.4 277.3 5.8 249 0 219.3 0c-28 0-55.1 5.3-81.2 15.8C111.9 26.4 88.6 41.3 68.2 60.5l-37.1-36.8c-5.7-5.9-12.3-7.2-19.7-4C3.8 22.9 0 28.6 0 36.5v127.9c0 4.9 1.8 9.2 5.4 12.8 3.6 3.6 7.9 5.4 12.9 5.4h127.9c8 0 13.6-3.8 16.8-11.4 3.2-7.4 1.9-14-4-19.7l-39.1-39.4c13.3-12.6 28.6-22.2 45.7-29 17.1-6.8 35-10.1 53.7-10.1 19.8 0 38.7 3.9 56.7 11.6 18 7.7 33.5 18.1 46.7 31.3 13.1 13.1 23.6 28.7 31.3 46.7 7.7 18 11.6 36.9 11.6 56.7 0 19.8-3.9 38.7-11.6 56.7 -7.7 18-18.1 33.5-31.3 46.7 -13.1 13.1-28.7 23.6-46.7 31.3 -18 7.7-36.9 11.6-56.7 11.6 -22.7 0-44.1-4.9-64.2-14.8 -20.2-9.9-37.2-23.9-51.1-42 -1.3-1.9-3.5-3-6.6-3.4 -2.9 0-5.2 0.9-7.1 2.6l-39.1 39.4c-1.5 1.5-2.3 3.5-2.4 5.9 -0.1 2.4 0.5 4.5 1.9 6.4 20.7 25.1 45.9 44.6 75.4 58.4 29.5 13.8 60.6 20.7 93.4 20.7 29.7 0 58.1-5.8 85.1-17.4 27-11.6 50.3-27.2 69.9-46.8 19.6-19.6 35.2-42.9 46.8-69.9s17.4-55.4 17.4-85.1C438.5 189.6 432.7 161.2 421.1 134.2z"/></svg>
  , 'right-arrow': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="445" height="445" viewBox="0 0 444.8 444.8"><path d="M352 196.7L165.9 10.8C159 3.6 150.5 0 140.2 0c-10.3 0-18.8 3.6-25.7 10.8L92.8 32.3c-7 7-10.6 15.6-10.6 25.7 0 9.9 3.5 18.6 10.6 26l138.8 138.5L92.8 361.2c-7 7-10.6 15.6-10.6 25.7 0 9.9 3.5 18.6 10.6 26l21.7 21.4c7 7 15.6 10.6 25.7 10.6 10.1 0 18.7-3.5 25.7-10.6l186.1-185.9c7-7.4 10.6-16.1 10.6-26C362.6 212.3 359.1 203.8 352 196.7z"/></svg>
  , 'link': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="466" height="466" viewBox="0 0 466 466"><path d="M442 284.4l-59.4-59.4c-16-16-35.4-24-58.2-24 -23.2 0-43 8.4-59.4 25.1l-25.1-25.1c16.8-16.4 25.1-36.3 25.1-59.7 0-22.8-7.9-42.2-23.7-58l-58.8-59.1c-15.8-16.2-35.2-24.3-58.2-24.3 -22.8 0-42.2 7.9-58 23.7L24.3 65.4C8.1 81.2 0 100.5 0 123.3c0 22.8 8 42.3 24 58.2l59.4 59.4c16 16 35.4 24 58.2 24 23.2 0 43-8.4 59.4-25.1l25.1 25.1c-16.7 16.4-25.1 36.3-25.1 59.7 0 22.8 7.9 42.2 23.7 58l58.8 59.1c15.8 16.2 35.2 24.3 58.2 24.3 22.8 0 42.2-7.9 58-23.7l42-41.7c16.2-15.8 24.3-35.1 24.3-58C465.9 319.8 458 300.4 442 284.4zM201 162.2c-0.6-0.6-2.3-2.4-5.3-5.4 -2.9-3-5-5.1-6.1-6.1 -1.1-1-3-2.5-5.4-4.3 -2.5-1.8-4.9-3-7.3-3.7 -2.4-0.7-5-1-7.9-1 -7.6 0-14.1 2.7-19.4 8 -5.3 5.3-8 11.8-8 19.4 0 2.9 0.3 5.5 1 7.9 0.7 2.4 1.9 4.8 3.7 7.3 1.8 2.5 3.2 4.3 4.3 5.4 1 1.1 3.1 3.2 6.1 6.1 3 3 4.9 4.7 5.4 5.3 -5.7 5.9-12.6 8.8-20.6 8.8 -7.8 0-14.3-2.6-19.4-7.7L62.8 142.8c-5.3-5.3-8-11.8-8-19.4 0-7.4 2.7-13.8 8-19.1l42-41.7c5.5-5.1 12-7.7 19.4-7.7 7.6 0 14.1 2.7 19.4 8l58.8 59.1c5.3 5.3 8 11.8 8 19.4C210.4 149.3 207.3 156.3 201 162.2zM403.1 361.7l-42 41.7c-5.3 4.9-11.8 7.4-19.4 7.4 -7.8 0-14.3-2.6-19.4-7.7l-58.8-59.1c-5.3-5.3-8-11.8-8-19.4 0-8 3.1-14.9 9.4-20.8 0.6 0.6 2.3 2.4 5.3 5.4 3 3 5 5.1 6.1 6.1 1.1 1.1 2.9 2.5 5.4 4.3 2.5 1.8 4.9 3 7.3 3.7 2.4 0.7 5 1 7.9 1 7.6 0 14.1-2.7 19.4-8 5.3-5.3 8-11.8 8-19.4 0-2.9-0.3-5.5-1-7.9 -0.7-2.4-1.9-4.8-3.7-7.3 -1.8-2.5-3.2-4.3-4.3-5.4 -1-1.1-3.1-3.2-6.1-6.1 -3-2.9-4.9-4.7-5.4-5.3 5.7-6.1 12.6-9.1 20.6-9.1 7.6 0 14.1 2.7 19.4 8l59.4 59.4c5.3 5.3 8 11.8 8 19.4C411.1 350 408.5 356.4 403.1 361.7z"/></svg>
  , 'unlink': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="475" height="475" viewBox="0 0 475.1 475.1"><path d="M107.1 317.2c1.7-1.7 2.6-3.9 2.6-6.6 0-2.7-0.9-4.9-2.6-6.6 -1.7-1.7-3.9-2.6-6.6-2.6H9.1c-2.7 0-4.9 0.9-6.6 2.6C0.9 305.8 0 308 0 310.6c0 2.7 0.9 4.9 2.6 6.6 1.7 1.7 3.9 2.6 6.6 2.6H100.5C103.2 319.8 105.4 318.9 107.1 317.2z"/><path d="M310.6 109.6c2.7 0 4.9-0.9 6.6-2.6 1.7-1.7 2.6-3.9 2.6-6.6V9.1c0-2.7-0.9-4.8-2.6-6.6 -1.7-1.7-3.9-2.6-6.6-2.6 -2.7 0-4.9 0.9-6.6 2.6 -1.7 1.7-2.6 3.9-2.6 6.6v91.4c0 2.7 0.9 4.9 2.6 6.6C305.8 108.8 308 109.6 310.6 109.6z"/><path d="M118.8 347.2c-2.5 0-4.7 0.9-6.6 2.6l-73.1 73.1c-1.7 1.9-2.6 4.1-2.6 6.6 0 2.5 0.9 4.7 2.6 6.6 2.1 1.7 4.3 2.6 6.6 2.6 2.5 0 4.7-0.9 6.6-2.6l73.1-73.1c1.7-1.9 2.6-4.1 2.6-6.6 0-2.5-0.9-4.7-2.6-6.6C123.4 348 121.2 347.2 118.8 347.2z"/><path d="M356.3 127.9c2.3 0 4.5-0.9 6.6-2.6l73.1-73.1c1.7-1.9 2.6-4.1 2.6-6.6 0-2.5-0.9-4.7-2.6-6.6 -1.9-1.7-4.1-2.6-6.6-2.6 -2.5 0-4.7 0.9-6.6 2.6l-73.1 73.1c-1.7 1.9-2.6 4.1-2.6 6.6 0 2.5 0.9 4.7 2.6 6.6C351.8 127.1 354 127.9 356.3 127.9z"/><path d="M350.6 193c-4-4-9.3-8-16-12l-5.1 68.2 78.2 78.5c5.3 5.3 8 11.8 8 19.4 0 7.4-2.7 13.8-8 19.1l-42 41.7c-5.1 5.1-11.6 7.7-19.4 7.6 -7.8-0.1-14.3-2.7-19.4-7.9l-77.9-78.2 -68.2 5.1c4 6.7 8 12 12 16l95.4 95.6c15.8 16.2 35.2 24.3 58.2 24.3 22.8 0 42.2-7.9 58-23.7l42-41.7c16.2-15.8 24.3-35.1 24.3-58 0-22.5-8-41.9-24-58.2L350.6 193z"/><path d="M472.5 157.9c-1.7-1.7-3.9-2.6-6.6-2.6h-91.4c-2.7 0-4.9 0.9-6.6 2.6 -1.7 1.7-2.6 3.9-2.6 6.6 0 2.7 0.9 4.9 2.6 6.6 1.7 1.7 3.9 2.6 6.6 2.6h91.4c2.7 0 4.9-0.9 6.6-2.6 1.7-1.7 2.6-3.9 2.6-6.6C475.1 161.8 474.2 159.6 472.5 157.9z"/><path d="M109.3 67.1c5.5-5.1 12-7.7 19.4-7.7 7.6 0 14.1 2.7 19.4 8l77.9 78.2 68.2-5.1c-4-6.7-8-12-12-16l-95.4-95.6c-15.8-16.2-35.2-24.3-58.2-24.3 -22.8 0-42.2 7.9-58 23.7L28.8 70C12.7 85.8 4.6 105.1 4.6 127.9c0 22.5 8 41.9 24 58.2l95.9 95.9c4 4 9.3 8 16 12l5.1-68.5L67.4 147.3c-5.3-5.3-8-11.8-8-19.4 0-7.4 2.7-13.8 8-19.1L109.3 67.1z"/><path d="M164.5 365.5c-2.7 0-4.9 0.9-6.6 2.6 -1.7 1.7-2.6 3.9-2.6 6.6v91.4c0 2.7 0.9 4.9 2.6 6.6 1.7 1.7 3.9 2.6 6.6 2.6 2.7 0 4.9-0.9 6.6-2.6 1.7-1.7 2.6-3.9 2.6-6.6v-91.4c0-2.7-0.9-4.9-2.6-6.6C169.3 366.3 167.1 365.5 164.5 365.5z"/></svg>
  , 'enter': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="439" height="439" viewBox="0 0 438.5 438.5"><path d="M414.4 60.7c-16.1-16.1-35.4-24.1-58.1-24.1h-91.4c-2.5 0-4.4 0.6-5.9 1.9 -1.4 1.2-2.4 3.1-2.9 5.6 -0.5 2.5-0.8 4.7-0.9 6.7 -0.1 2-0.1 4.5 0.1 7.6 0.2 3 0.3 4.9 0.3 5.7 0.6 1.5 0.8 2.8 0.6 3.9 -0.2 1 0.5 1.9 2 2.6 1.5 0.7 2.3 1.2 2.3 1.6 0 0.4 1.1 0.7 3.3 0.9 2.2 0.2 3.3 0.3 3.3 0.3h3.7 3.1 82.2c12.6 0 23.3 4.5 32.3 13.4 9 8.9 13.4 19.7 13.4 32.3V319.8c0 12.6-4.5 23.3-13.4 32.3 -8.9 8.9-19.7 13.4-32.3 13.4h-91.4c-2.5 0-4.4 0.6-5.9 1.9 -1.4 1.2-2.4 3.1-2.9 5.6 -0.5 2.5-0.8 4.7-0.9 6.7 -0.1 2-0.1 4.5 0.1 7.6 0.2 3 0.3 4.9 0.3 5.7 0 2.5 0.9 4.6 2.7 6.4 1.8 1.8 3.9 2.7 6.4 2.7h91.4c22.6 0 42-8 58.1-24.1 16.1-16.1 24.1-35.4 24.1-58.1V118.8C438.5 96.1 430.5 76.8 414.4 60.7z"/><path d="M338 219.3c0-4.9-1.8-9.2-5.4-12.8L177.3 51.1c-3.6-3.6-7.9-5.4-12.8-5.4 -5 0-9.2 1.8-12.8 5.4 -3.6 3.6-5.4 7.9-5.4 12.9v82.2H18.3c-5 0-9.2 1.8-12.9 5.4C1.8 155.2 0 159.5 0 164.5v109.6c0 4.9 1.8 9.2 5.4 12.8 3.6 3.6 7.9 5.4 12.9 5.4h127.9v82.2c0 4.9 1.8 9.2 5.4 12.8 3.6 3.6 7.9 5.4 12.9 5.4 4.9 0 9.2-1.8 12.8-5.4l155.3-155.3C336.2 228.5 338 224.2 338 219.3z"/></svg>
  , 'leave': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="448" height="448" viewBox="0 0 447.7 447.7"><path d="M182.7 379.2c-0.6-1.5-0.8-2.8-0.6-3.9 0.2-1-0.5-1.9-2-2.6 -1.5-0.7-2.3-1.2-2.3-1.6 0-0.4-1.1-0.7-3.3-0.9 -2.2-0.2-3.3-0.3-3.3-0.3h-3.7 -3.1H82.2c-12.6 0-23.3-4.5-32.3-13.4 -8.9-8.9-13.4-19.7-13.4-32.3V123.3c0-12.6 4.5-23.3 13.4-32.3 8.9-8.9 19.7-13.4 32.3-13.4h91.4c2.5 0 4.4-0.6 5.9-1.9 1.4-1.2 2.4-3.1 2.9-5.6 0.5-2.5 0.8-4.7 0.9-6.7 0.1-2 0-4.5-0.1-7.6 -0.2-3-0.3-4.9-0.3-5.7 0-2.5-0.9-4.6-2.7-6.4 -1.8-1.8-3.9-2.7-6.4-2.7H82.2c-22.6 0-42 8-58.1 24.1C8 81.3 0 100.7 0 123.3v201c0 22.6 8 42 24.1 58.1 16.1 16.1 35.5 24.1 58.1 24.1h91.4c2.5 0 4.4-0.6 5.9-1.9 1.4-1.2 2.4-3.1 2.9-5.6 0.5-2.5 0.8-4.7 0.9-6.7 0.1-2 0-4.5-0.1-7.6C182.8 381.8 182.7 379.9 182.7 379.2z"/><path d="M442.2 211L286.9 55.7c-3.6-3.6-7.9-5.4-12.8-5.4 -4.9 0-9.2 1.8-12.9 5.4 -3.6 3.6-5.4 7.9-5.4 12.9v82.2H127.9c-5 0-9.2 1.8-12.8 5.4 -3.6 3.6-5.4 7.9-5.4 12.9v109.6c0 4.9 1.8 9.2 5.4 12.8 3.6 3.6 7.9 5.4 12.9 5.4h127.9v82.2c0 4.9 1.8 9.2 5.4 12.8 3.6 3.6 7.9 5.4 12.9 5.4 4.9 0 9.2-1.8 12.8-5.4L442.2 236.7c3.6-3.6 5.4-7.9 5.4-12.8C447.7 218.9 445.9 214.6 442.2 211z"/></svg>
  , 'search': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="475" height="475" viewBox="0 0 475.1 475.1"><path d="M464.5 412.8l-97.9-97.9c23.6-34.1 35.4-72 35.4-113.9 0-27.2-5.3-53.2-15.9-78.1 -10.6-24.8-24.8-46.3-42.8-64.2 -18-18-39.4-32.3-64.2-42.8C254.2 5.3 228.2 0 201 0c-27.2 0-53.2 5.3-78.1 15.8C98.1 26.4 76.7 40.7 58.7 58.7c-18 18-32.3 39.4-42.8 64.2C5.3 147.8 0 173.8 0 201c0 27.2 5.3 53.2 15.8 78.1 10.6 24.8 24.8 46.2 42.8 64.2 18 18 39.4 32.3 64.2 42.8 24.8 10.6 50.9 15.8 78.1 15.8 41.9 0 79.9-11.8 113.9-35.4l97.9 97.6c6.9 7.2 15.4 10.8 25.7 10.8 9.9 0 18.5-3.6 25.7-10.8 7.2-7.2 10.8-15.8 10.8-25.7C475.1 428.5 471.6 419.9 464.5 412.8zM291.4 291.4c-25 25-55.1 37.5-90.4 37.5 -35.2 0-65.3-12.5-90.4-37.5 -25-25-37.5-55.1-37.5-90.4 0-35.2 12.5-65.3 37.5-90.4 25-25 55.2-37.5 90.4-37.5 35.2 0 65.3 12.5 90.4 37.5 25 25 37.5 55.2 37.5 90.4C328.9 236.2 316.4 266.3 291.4 291.4z"/></svg>
  , 'loading': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="457" height="457" viewBox="0 0 456.8 456.8"><path d="M109.6 324.3c-11.4 0-21.1 4-29.1 12 -8 8-12 17.7-12 29.1 0 11.4 4 21.1 12 29.1 8 8 17.7 12 29.1 12 11.2 0 20.9-4 29-12 8.1-8 12.1-17.7 12.1-29.1 0-11.4-4-21.1-12.1-29.1C130.5 328.3 120.9 324.3 109.6 324.3z"/><path d="M100.5 237.5c0-12.6-4.5-23.3-13.4-32.3 -8.9-8.9-19.7-13.4-32.3-13.4 -12.6 0-23.3 4.5-32.3 13.4 -8.9 8.9-13.4 19.7-13.4 32.3 0 12.6 4.5 23.3 13.4 32.3 8.9 8.9 19.7 13.4 32.3 13.4 12.6 0 23.3-4.5 32.3-13.4C96 260.9 100.5 250.1 100.5 237.5z"/><path d="M365.5 132.5c6.3 0 11.7-2.2 16.1-6.7 4.5-4.5 6.7-9.9 6.7-16.1 0-6.3-2.2-11.7-6.7-16.1 -4.5-4.5-9.9-6.7-16.1-6.7 -6.3 0-11.7 2.2-16.1 6.7 -4.5 4.5-6.7 9.8-6.7 16.1s2.2 11.7 6.7 16.1C353.8 130.2 359.2 132.5 365.5 132.5z"/><path d="M109.6 59.4c-13.9 0-25.7 4.9-35.5 14.7 -9.8 9.8-14.7 21.7-14.7 35.5 0 13.9 4.9 25.7 14.7 35.5 9.8 9.8 21.7 14.7 35.5 14.7s25.7-4.9 35.5-14.7c9.8-9.8 14.7-21.7 14.7-35.5 0-13.9-4.9-25.7-14.7-35.5C135.4 64.3 123.5 59.4 109.6 59.4z"/><path d="M439.7 218.1c-5.3-5.3-11.8-8-19.4-8 -7.6 0-14.1 2.7-19.4 8 -5.3 5.3-8 11.8-8 19.4 0 7.6 2.7 14.1 8 19.4 5.3 5.3 11.8 8 19.4 8 7.6 0 14.1-2.7 19.4-8 5.3-5.3 8-11.8 8-19.4C447.7 229.9 445 223.5 439.7 218.1z"/><path d="M365.5 333.5c-8.8 0-16.3 3.1-22.6 9.4 -6.3 6.3-9.4 13.8-9.4 22.6 0 8.8 3.1 16.3 9.4 22.6 6.3 6.3 13.8 9.4 22.6 9.4 8.8 0 16.3-3.1 22.6-9.4 6.3-6.3 9.4-13.8 9.4-22.6 0-8.8-3.1-16.3-9.4-22.6C381.7 336.6 374.2 333.5 365.5 333.5z"/><path d="M237.5 383.7c-10.1 0-18.7 3.6-25.8 10.7 -7.1 7.1-10.7 15.7-10.7 25.8s3.6 18.7 10.7 25.8c7.1 7.1 15.8 10.7 25.8 10.7 10.1 0 18.7-3.6 25.8-10.7 7.1-7.1 10.7-15.7 10.7-25.8s-3.6-18.7-10.7-25.8S247.6 383.7 237.5 383.7z"/><path d="M237.5 0c-15.2 0-28.2 5.3-38.8 16 -10.7 10.7-16 23.6-16 38.8 0 15.2 5.3 28.2 16 38.8 10.7 10.7 23.6 16 38.8 16 15.2 0 28.2-5.3 38.8-16 10.7-10.7 16-23.6 16-38.8 0-15.2-5.3-28.2-16-38.8C265.7 5.3 252.8 0 237.5 0z"/></svg> 
  , 'upload': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="475" height="475" viewBox="0 0 475.1 475.1"><path d="M467.1 327.8c-5.3-5.3-11.8-8-19.4-8h-121.9c-4 10.7-10.7 19.4-20.1 26.3 -9.4 6.9-19.9 10.3-31.5 10.3h-73.1c-11.6 0-22.1-3.4-31.5-10.3 -9.4-6.9-16.1-15.6-20.1-26.3H27.4c-7.6 0-14.1 2.7-19.4 8C2.7 333.1 0 339.6 0 347.2v91.4c0 7.6 2.7 14.1 8 19.4 5.3 5.3 11.8 8 19.4 8h420.3c7.6 0 14.1-2.7 19.4-8 5.3-5.3 8-11.8 8-19.4v-91.4C475.1 339.6 472.4 333.1 467.1 327.8zM360 424c-3.6 3.6-7.9 5.4-12.9 5.4s-9.2-1.8-12.8-5.4c-3.6-3.6-5.4-7.9-5.4-12.8s1.8-9.2 5.4-12.8c3.6-3.6 7.9-5.4 12.8-5.4s9.2 1.8 12.9 5.4c3.6 3.6 5.4 7.9 5.4 12.8S363.6 420.4 360 424zM433.1 424c-3.6 3.6-7.9 5.4-12.8 5.4 -4.9 0-9.2-1.8-12.8-5.4 -3.6-3.6-5.4-7.9-5.4-12.8s1.8-9.2 5.4-12.8c3.6-3.6 7.9-5.4 12.8-5.4 4.9 0 9.2 1.8 12.8 5.4 3.6 3.6 5.4 7.9 5.4 12.8S436.7 420.4 433.1 424z"/><path d="M109.6 173.6h73.1v127.9c0 4.9 1.8 9.2 5.4 12.8 3.6 3.6 7.9 5.4 12.8 5.4h73.1c4.9 0 9.2-1.8 12.8-5.4 3.6-3.6 5.4-7.9 5.4-12.8V173.6h73.1c8 0 13.6-3.8 16.8-11.4 3.2-7.4 1.9-14-4-19.7L250.4 14.6c-3.4-3.6-7.7-5.4-12.8-5.4 -5.1 0-9.4 1.8-12.8 5.4L96.8 142.5c-5.9 5.7-7.2 12.3-4 19.7C96 169.8 101.6 173.6 109.6 173.6z"/></svg>
  , 'expand': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="445" height="445" viewBox="0 0 444.8 444.8"><path d="M434.3 114.2l-21.4-21.4c-7.4-7-16.1-10.6-26-10.6 -10.1 0-18.7 3.5-25.7 10.6L222.4 231.5 83.7 92.8c-7-7-15.6-10.6-25.7-10.6 -9.9 0-18.6 3.5-26 10.6l-21.1 21.4C3.6 121.4 0 130.1 0 140.2c0 10.3 3.6 18.8 10.8 25.7l185.9 185.9c6.9 7.2 15.4 10.8 25.7 10.8 10.1 0 18.8-3.6 26-10.8l185.9-185.9c7-7 10.6-15.6 10.6-25.7C444.8 130.3 441.3 121.6 434.3 114.2z"/></svg>
  , 'collapse': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="445" height="445" viewBox="0 0 444.8 444.8"><path d="M434 278.7L248.4 92.8c-7.4-7-16.1-10.6-26-10.6 -10.1 0-18.7 3.5-25.7 10.6L10.8 278.7C3.6 285.9 0 294.5 0 304.6c0 10.3 3.6 18.8 10.8 25.7l21.4 21.4c6.9 7.2 15.4 10.9 25.7 10.9 10.3 0 18.8-3.6 25.7-10.9L222.4 213.3 361.2 351.7c6.8 7.2 15.4 10.9 25.7 10.9 10.1 0 18.7-3.6 26-10.9l21.4-21.4c7-7 10.6-15.6 10.6-25.7C444.8 294.5 441.2 285.9 434 278.7z"/></svg>
  , 'play': cn => <svg className={cn} xmlns="http://www.w3.org/2000/svg" width="439" height="439" viewBox="0 0 438.5 438.5"><path d="M409.1 109.2c-19.6-33.6-46.2-60.2-79.8-79.8C295.7 9.8 259.1 0 219.3 0c-39.8 0-76.5 9.8-110.1 29.4 -33.6 19.6-60.2 46.2-79.8 79.8C9.8 142.8 0 179.5 0 219.3s9.8 76.5 29.4 110.1c19.6 33.6 46.2 60.2 79.8 79.8 33.6 19.6 70.3 29.4 110.1 29.4s76.5-9.8 110.1-29.4c33.6-19.6 60.2-46.2 79.8-79.8 19.6-33.6 29.4-70.3 29.4-110.1C438.5 179.5 428.7 142.8 409.1 109.2zM328.9 235L173.6 326.3c-2.9 1.7-5.9 2.6-9.1 2.6 -3 0-6.1-0.8-9.1-2.3 -6.1-3.6-9.1-8.9-9.1-16V127.9c0-7 3-12.4 9.1-16 6.3-3.4 12.4-3.3 18.3 0.3l155.3 91.4c6.1 3.4 9.1 8.7 9.1 15.7C338 226.3 335 231.5 328.9 235z"/></svg>
  }

export type IconType = 'edit' | 'delete' | 'refresh' | 'question' | 'add' | 'cancel' | 'ok' | 'info' | 'alert' | 'remove' | 'undo' | 'right-arrow' | 'link' | 'unlink' | 'enter' | 'leave' | 'search' | 'loading' | 'upload' | 'expand' | 'collapse' | 'play'

export const Icon = (props: {type: IconType, className?: string}) => {
  const icon = icons[props.type] || icons.question
  return icon(props.className)
}
import * as React from 'react';
import * as classNames from 'classnames'

export interface HasProductFlags { biodynamic: boolean
                                 , fairTrade: boolean
                                 , glutenFree: boolean
                                 , organic: boolean
                                 , addedSugar: boolean
                                 , vegan: boolean
                                 }

export let ProductFlags = ({p}: {p: HasProductFlags}) => 
  <span>
    <span className={classNames("inline-block mr-1 w-4 h-4 text-center", { "text-white bg-grey-dark": p.biodynamic, "text-grey-dark": !p.biodynamic })}><span className="inline-block">B</span></span>
    <span className={classNames("inline-block mr-1 w-4 h-4 text-center", { "text-white bg-grey-dark": p.glutenFree, "text-grey-dark": !p.glutenFree })}><span className="inline-block">G</span></span>
    <span className={classNames("inline-block mr-1 w-4 h-4 text-center", { "text-white bg-grey-dark": p.organic,    "text-grey-dark": !p.organic    })}><span className="inline-block">O</span></span>
    <span className={classNames("inline-block mr-1 w-4 h-4 text-center", { "text-white bg-grey-dark": p.fairTrade,  "text-grey-dark": !p.fairTrade  })}><span className="inline-block">F</span></span>
    <span className={classNames("inline-block mr-1 w-4 h-4 text-center", { "text-white bg-grey-dark": p.vegan,      "text-grey-dark": !p.vegan      })}><span className="inline-block">V</span></span>
    <span className={classNames("inline-block mr-1 w-4 h-4 text-center", { "text-white bg-grey-dark": p.addedSugar, "text-grey-dark": !p.addedSugar })}><span className="inline-block">S</span></span>
  </span>
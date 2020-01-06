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
    <span className={classNames("inline-block mr-1 w-4 h-4 text-center", { "text-white bg-grey": p.biodynamic, "text-grey": !p.biodynamic })}><span className="inline-block">B</span></span>
    <span className={classNames("inline-block mr-1 w-4 h-4 text-center", { "text-white bg-grey": p.glutenFree, "text-grey": !p.glutenFree })}><span className="inline-block">G</span></span>
    <span className={classNames("inline-block mr-1 w-4 h-4 text-center", { "text-white bg-grey": p.organic,    "text-grey": !p.organic    })}><span className="inline-block">O</span></span>
    <span className={classNames("inline-block mr-1 w-4 h-4 text-center", { "text-white bg-grey": p.fairTrade,  "text-grey": !p.fairTrade  })}><span className="inline-block">F</span></span>
    <span className={classNames("inline-block mr-1 w-4 h-4 text-center", { "text-white bg-grey": p.vegan,      "text-grey": !p.vegan      })}><span className="inline-block">V</span></span>
    <span className={classNames("inline-block mr-1 w-4 h-4 text-center", { "text-white bg-grey": p.addedSugar, "text-grey": !p.addedSugar })}><span className="inline-block">S</span></span>
  </span>
import * as React from 'react';
import * as ReactDOM from 'react-dom';

import { Main } from './Main';

import '../static/dist/styles.css';

(function(history){
    var pushState = history.pushState;
    history.pushState = function(state, title, url) {
        let h = (history as any)
        if (typeof h.onpushstate == "function") {
            h.onpushstate(state, title, url);
        }
        return pushState.apply(history, arguments);
    }
})(window.history);

ReactDOM.render(
    <Main />,
    document.getElementById('app')
);
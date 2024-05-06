
function createEditUrl({ module, document, bizId }) {

    return `./?a=e&m=${module}&d=${document}&i=${bizId}`;
}

/**
 * Open an edit view for the request document in this window.
 * 
 * @param {Object} obj Parameter object containing: module, document and bizId
 */
export function openDocInSameWindow({ module, document, bizId }) {

    const url = createEditUrl({ module, document, bizId });

    if (!!SKYVE && SKYVE?.PF?.pushHistory) {
        SKYVE.PF.pushHistory(url)
    } else {
        console.warn('openDocInNewWindow(): SKYVE.PF.pushHistory not available', url);
    }
}

/**
 * Open a new window with an edit view for the request document.
 * 
 * @param {Object} obj Parameter object containing: module, document and bizId
 */
export function openDocInNewWindow({ module, document, bizId }) {

    const url = createEditUrl({ module, document, bizId });
    window.open(url, '_blank');
}
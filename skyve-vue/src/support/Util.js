
/**
 * Create an edit url for the given module/doc/bizId. Omit bizId
 * to get a url for creating a new document.
 * 
 * @param {*} param0 
 * @returns 
 */
function createEditUrl({ module, document, bizId = '' }) {

    let url = `./?a=e&m=${module}&d=${document}`;
    if ((bizId ?? '').length > 0) {
        url += `&i=${bizId}`;
    }

    return url;
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

/**
 * Convert back from a SmartClient sanitised binding to something 
 * we can send to the content/resource servlet.
 * 
 * Adapted from BindUtil.
 * 
 * @param {String} binding The SC binding
 * @returns {String} The original, unsanitised, binding
 */
export function unsanitiseBinding(binding = '') {
    let result = binding.replaceAll(/_(\d*)_/g, '\[$1\]');
    result = result.replaceAll('_', '.');
    return result;
}
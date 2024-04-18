const SNAP_PATH = 'smartsnap';
const FORM_URL_ENCODED = 'application/x-www-form-urlencoded;charset=UTF-8';

const params = {
    action: "a",
    document: "d",
    name: "n",
    csrf: "_csrf",
    snapshot: "s",
    id: "i"
};

const actions = {
    list: "L",
    delete: "D",
    update: "U",
    create: "N"
};

/**
 * Predicate function to filter out invalid/unusable 
 * snapshots.
 * 
 * @param {*} input Snapshot entry
 * @returns true if we can handle this entry
 */
function _filterSnapshots(input) {

    const fieldState = input?.snapshot?.fieldState;
    if (!fieldState) {
        return false;
    }

    try {
        JSON.parse(fieldState);
    } catch (err) {
        return false;
    }

    return true;
}


export const SnapshotService = {

    async getSnapshots({ documentQuery, filterInvalid = true}) {

        const fd = new FormData();
        fd.append(params.action, actions.list);
        fd.append(params.document, documentQuery);

        const req = new Request(SNAP_PATH, {
            method: 'POST',
            body: new URLSearchParams(fd),
            headers: {
                'Content-Type': FORM_URL_ENCODED
            }
        });

        const response = await fetch(req);
        const payload = await response.json();

        // Payload should be an array of objects
        // each with bizId, name, and snapshot props
        // We may want to filter out results we
        // can't use
        let snapshotFilter = () => true;
        if (filterInvalid) {
            snapshotFilter = _filterSnapshots;
        }

        const usableResults = payload.filter(snapshotFilter);
        console.debug(`getSnapshots: Got ${payload.length} results; ${usableResults.length} usable`);
        return usableResults;
    },
    async createSnapshot({ documentQuery, name, snapshot }) {

        const snapString = JSON.stringify(snapshot);

        const fd = new FormData();
        fd.append(params.action, actions.create);
        fd.append(params.name, name);
        fd.append(params.document, documentQuery);
        fd.append(params.snapshot, snapString);
        // CSRF not needed by the looks...
        // fd.append(params.csrf, '???');

        const req = new Request(SNAP_PATH, {
            method: 'POST',
            body: new URLSearchParams(fd),
            headers: {
                'Content-Type': FORM_URL_ENCODED
            }
        });

        const response = await fetch(req);
        const json = await response.json();

        // Result should looks like { "bizId": "xyz-abc" }
        console.debug('Created snapshot', json);
    },
    async deleteSnapshot({ id, documentQuery }) {

        const fd = new FormData();
        fd.append(params.action, actions.delete);
        fd.append(params.document, documentQuery);
        fd.append(params.id, id);

        const req = new Request(SNAP_PATH, {
            method: 'POST',
            body: new URLSearchParams(fd),
            headers: {
                'Content-Type': FORM_URL_ENCODED
            }
        });

        const response = await fetch(req);
        let status = response.status
        if (status == 200) {
            console.debug(`Deleted snapshot ${id}`);
            return true;
        } else {
            // Doesn't look like this'll hit in typical use
            console.warn(`Could not delete ${id}`);
            return false;
        }
    }, 
    async updateSnapshot({ snapshot, id }) {

        const snapString = JSON.stringify(snapshot);

        const fd = new FormData();
        fd.append(params.action, actions.update);
        fd.append(params.snapshot, snapString);
        fd.append(params.id, id);

        const req = new Request(SNAP_PATH, {
            method: 'POST',
            body: new URLSearchParams(fd),
            headers: {
                'Content-Type': FORM_URL_ENCODED
            }
        });

        const response = await fetch(req);
        let status = response.status
        if (status == 200) {
            console.debug(`Updated snapshot ${id}`);
            return true;
        } else {
            // Doesn't look like this'll hit in typical use
            console.warn(`Could not update ${id}`);
            return false;
        }
    }
}
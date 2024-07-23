const SNAP_PATH = 'smartsnap';
const FORM_URL_ENCODED = 'application/x-www-form-urlencoded;charset=UTF-8';
const DEFAULT_TYPE = 'pf1';

const params = {
    action: "a",
    document: "d",
    name: "n",
    csrf: "_csrf",
    snapshot: "s",
    id: "i",
    type: "type"
};

const actions = {
    list: "L",
    delete: "D",
    update: "U",
    create: "N"
};

export const SnapshotService = {

    async getSnapshots({ documentQuery, type = DEFAULT_TYPE }) {

        const fd = new FormData();
        fd.append(params.action, actions.list);
        fd.append(params.document, documentQuery);
        fd.append(params.type, type);

        const req = new Request(SNAP_PATH, {
            method: 'POST',
            body: new URLSearchParams(fd),
            headers: {
                'Content-Type': FORM_URL_ENCODED
            }
        });

        const response = await fetch(req);
        let payload;
        try {
            payload = await response.json();
        } catch (err) {
            throw new Error('Error retrieving snapshots', { cause: err });
        }

        console.debug(`getSnapshots: Got ${payload.length} results`);
        return payload;
    },
    async createSnapshot({ documentQuery, name, snapshot, type = DEFAULT_TYPE }) {

        const snapString = JSON.stringify(snapshot);

        const fd = new FormData();
        fd.append(params.action, actions.create);
        fd.append(params.name, name);
        fd.append(params.document, documentQuery);
        fd.append(params.snapshot, snapString);
        fd.append(params.type, type);

        const req = new Request(SNAP_PATH, {
            method: 'POST',
            body: new URLSearchParams(fd),
            headers: {
                'Content-Type': FORM_URL_ENCODED
            }
        });

        const response = await fetch(req);
        // Result should looks like { "bizId": "xyz-abc" }
        return await response.json();
    },
    async deleteSnapshot({ id }) {

        const fd = new FormData();
        fd.append(params.action, actions.delete);
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
const SNAP_PATH = 'smartsnap';
const FORM_URL_ENCODED = 'application/x-www-form-urlencoded;charset=UTF-8';
const DEFAULT_TYPE = 'vue';

const params = {
    action: "a",
    document: "d",
    name: "n",
    csrf: "_csrf",
    snapshot: "s",
    id: "i",
    type: "t"
};

const actions = {
    list: "L",
    delete: "D",
    update: "U",
    create: "N"
};

// The session's CSRF token, from the X-CSRF-TOKEN header of the last response (the snapshot list comes first);
// the server refuses a create, update or delete without it
let csrfToken = null;

async function post(fd) {
    if (csrfToken) {
        fd.append(params.csrf, csrfToken);
    }
    const response = await fetch(new Request(SNAP_PATH, {
        method: 'POST',
        body: new URLSearchParams(fd),
        headers: {
            'Content-Type': FORM_URL_ENCODED
        }
    }));
    csrfToken = response.headers.get('X-CSRF-TOKEN') ?? csrfToken;
    return response;
}

export const SnapshotService = {

    async getSnapshots({ documentQuery, type = DEFAULT_TYPE }) {

        const fd = new FormData();
        fd.append(params.action, actions.list);
        fd.append(params.document, documentQuery);
        fd.append(params.type, type);

        const response = await post(fd);
        let payload;
        try {
            payload = await response.json();
        } catch (err) {
            throw new Error('Error retrieving snapshots', { cause: err });
        }

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

        const response = await post(fd);
        // Result should looks like { "bizId": "xyz-abc" }
        return await response.json();
    },
    async deleteSnapshot({ id }) {

        const fd = new FormData();
        fd.append(params.action, actions.delete);
        fd.append(params.id, id);

        const response = await post(fd);
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
    async updateSnapshot({ snapshot, id, type = DEFAULT_TYPE }) {

        const snapString = JSON.stringify(snapshot);

        const fd = new FormData();
        fd.append(params.action, actions.update);
        fd.append(params.snapshot, snapString);
        fd.append(params.id, id);
        fd.append(params.type, type);

        const response = await post(fd);
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
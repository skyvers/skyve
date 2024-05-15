<script>
import { PrimeIcons } from 'primevue/api';
import { SnapshotService } from '@/service/SnapshotService';
import Button from 'primevue/button';

export default {
    props: {
        documentQuery: String,
        snapshotState: Object,
        initialSelection: {
            type: String,
            default: null
        }
    },
    data() {
        return {
            snapshots: [],
            selectedSnapshot: null,
            showDialog: false,
            snapshotName: ''
        };
    },
    computed: {
        buttonLabel() {
            const label = this.selectedSnapshot?.name ?? 'None'
            return 'Snapshot: ' + label;
        },
        disableCreateSubmit() {
            if (!this.snapshotName) {
                return true;
            }

            return (this.snapshotName.length > 255);
        },
        items() {

            // Always available actions
            let menuItems = [
                {
                    label: 'New Snapshot',
                    icon: PrimeIcons.PLUS,
                    command: this.cofirmCreate
                },
                {
                    label: 'No Snapshot',
                    icon: PrimeIcons.EYE_SLASH,
                    command: this.clearSnapshot,
                    disabled: !this.selectedSnapshot
                },
            ];

            // Actions for each snapshot coming 
            // from the server
            this.snapshots
                .map(currSnap => {

                    // Only one of Update or Select will be available
                    let optionalAction;
                    if (currSnap.bizId == this?.selectedSnapshot?.bizId) {
                        optionalAction = {
                            label: 'Update Snapshot',
                            icon: PrimeIcons.PENCIL,
                            command: () => this.updateSnapshot(currSnap)
                        };
                    } else {
                        optionalAction = {
                            label: 'Select Snapshot',
                            icon: PrimeIcons.CHECK,
                            command: () => this.chooseSnapshot(currSnap)
                        }
                    }

                    let result = {
                        label: currSnap.name,
                        icon: PrimeIcons.CAMERA,
                        items: [
                            optionalAction,
                            {
                                label: 'Delete Snapshot',
                                icon: PrimeIcons.DELETELEFT,
                                command: () => this.deleteSnapshot(currSnap)
                            },
                        ]
                    };

                    return result;
                })
                .forEach(s => menuItems.push(s));

            return menuItems;
        }
    },
    methods: {
        toggle(event) {
            this.$refs.menu.toggle(event);
        },
        async reload() {

            const queryObj = { documentQuery: this.documentQuery };
            this.snapshots = await SnapshotService.getSnapshots(queryObj);
        },
        chooseSnapshot(snapshot) {
            this.selectedSnapshot = snapshot ?? null;
        },
        cofirmCreate() {
            this.showDialog = true;
        },
        async createSnapshot() {

            this.showDialog = false;
            const name = this.snapshotName;
            this.snapshotName = '';

            try {
                const result = await SnapshotService.createSnapshot({
                    documentQuery: this.documentQuery,
                    name: name,
                    snapshot: this.snapshotState
                });

                await this.reload();

                const newSnapshot = this.snapshots.find(snap => snap.bizId == result.bizId);
                this.selectedSnapshot = newSnapshot;

                return newSnapshot;
            } catch (err) {
                console.warn('Failed to create snapshot', this.snapshotState);
                return null;
            }
        },
        deleteSnapshot(currSnap) {
            return SnapshotService.deleteSnapshot({ id: currSnap.bizId })
                .then(() => this.reload())
                .then(() => {
                    if (this.selectedSnapshot?.bizId == currSnap?.bizId) {
                        this.clearSnapshot();
                    }
                });
        },
        async updateSnapshot(currSnap) {
            await SnapshotService.updateSnapshot({
                id: currSnap.bizId,
                snapshot: this.snapshotState
            })

            // The snapshot we just updated will
            // need to be refreshed
            await this.reload();
        },
        clearSnapshot() {
            this.selectedSnapshot = null;
        },
        cancelDialog() {
            this.showDialog = false
            this.snapshotName = '';
        }
    },
    watch: {
        selectedSnapshot(newSnap, oldSnap) {
            this.$emit('snapshotChanged', newSnap);
        },
    },
    emits: ['snapshotChanged'],
    mounted() {
        this.reload()
            .then(() => {

                // If an initialSelection bizId was provided, try to find the 
                // matching snapshot during this initial mounting of the picker
                if (!!this.initialSelection) {
                    const selection = this.snapshots
                        .find(snap => snap.bizId == this.initialSelection);
                    this.chooseSnapshot(selection);
                }
            });
    },
}
</script>

<template>
    <Button
        type="button"
        :label="buttonLabel"
        @click="toggle"
        aria-haspopup="true"
        aria-controls="overlay_tmenu"
    />
    <TieredMenu
        ref="menu"
        id="overlay_tmenu"
        :model="items"
        popup
    >
    </TieredMenu>
    <Dialog
        :visible="showDialog"
        modal
        header="Enter the new Snapshot name"
        @update:visible="cancelDialog"
    >
        <div class="flex flex-column gap-2">
            <label for="snapName">Snapshot name</label>
            <InputText
                id="snapName"
                v-model="snapshotName"
            />
            <div class="flex flex-row gap-2">
                <Button
                    type="button"
                    label="Cancel"
                    severity="secondary"
                    @click="cancelDialog"
                />
                <Button
                    type="button"
                    label="Save"
                    @click="createSnapshot"
                    :disabled="disableCreateSubmit"
                />
            </div>
        </div>
    </Dialog>
</template>
<style scoped></style>
<script>
import Column from 'primevue/column';
import { FilterOperator } from 'primevue/api';
import { MatchModes } from './support/MatchModes';
import Dropdown from 'primevue/dropdown';
import { openDocInNewWindow, openDocInSameWindow, SNAP_KEY_PREFIX } from './support/Util';
import { applyConverters } from './support/Converters';

/**
 * Map from the skyve attribute type to
 * the default filter operator to use for 
 * that column.
 */
function defaultMatchMode(columnType) {

    return (MatchModes[columnType] ?? [MatchModes.MODES.EQUALS])[0].value;
}

/**
 * Compare the two provided arrays for equality;
 * ie: same length, and contents are equal 
 * accoring to ==.
 */
function arraysEqual(a, b) {

    if (!Array.isArray(a) || !Array.isArray(b)) {
        return false;
    }

    if (a.length != b.length) {
        return false;
    }

    return a.every((val, index) => val == b[index])
}

/**
 * Call the provided RemoteCommand function, converting the 
 * supplied parameters object from `{ k: v }` to 
 * `[{ name: k, value: v }]`.
 * 
 * Returns the result of the provided command (presumably a 
 * Promise)
 * 
 * @param {String} commandName The name of the function (in 
 * global scope) that will be called.
 * @param {Object} paramsObj 
 */
async function callRemoteCommand(commandName, paramsObj) {

    const commandFn = window[commandName];
    if (!commandFn) {
        throw `CommandName (${commandName}) not found`;
    }

    let paramsArray = [];
    for (let key in paramsObj) {
        paramsArray.push({ name: key, value: paramsObj[key] });
    }

    return commandFn(paramsArray);
}

export default {
    props: {
        owningModule: String,
        owningDocument: String,
        drivingModule: String,
        drivingDocument: String,
        query: String,
        model: String,
        title: String,
        columns: Object,
        dateFormat: {
            type: String,
            default: 'dd/mm/yy'
        },
        dateTimeFormat: {
            type: String,
            default: 'dd/mm/yy'
        },
        hourFormat: {
            type: String,
            default: '24'
        },
        contextId: {
            type: String,
            default: null
        },
        actions: {
            type: Object,
            default: {
                selected: null,
                edited: null,
                deleted: null
            }
		},
        showAdd: {
            type: Boolean,
            default: true
        },
        showZoom: {
            type: Boolean,
            default: true
        },
        showFilter: {
            type: Boolean,
            default: true
        },
        showSummary: {
            type: Boolean,
            default: true
        },
        showSnap: {
            type: Boolean,
            default: true
        }
    },
    data() {
        return {
            loading: true,
            loadTimeout: null,
            value: [],
            totalRecords: 0,
            filters: {},
            smartClientCriteria: null,
            firstRow: 0,
            pageSize: 25,
            errorText: '',

            multiSortMeta: [],

            selectedColumns: [],
            columnOrder: [],
            columnWidths: [],

            summarySelection: '',
            summaryOpts: ['', 'Count', 'Avg', 'Sum', 'Min', 'Max'],
            summaryRow: {},

            topLevelOperators: [
                {
                    label: 'Match All',
                    value: 'and'
                },
                {
                    label: 'Match Any',
                    value: 'or'
                }
            ],
            selectedTopLevelOperator: 'and',

            matchModes: MatchModes,

            // Support params for row right click context menu:
            selectedRow: null,
            menuModel: [
                {
                    label: 'View Detail',
                    icon: 'pi pi-angle-right',
                    command: () => openDocInSameWindow({
                        bizId: this.selectedRow.bizId,
                        module: this.selectedRow.bizModule,
                        document: this.selectedRow.bizDocument
                    })
                },
                {
                    label: 'Popout Detail',
                    icon: 'pi pi-external-link',
                    command: () => openDocInNewWindow({
                        bizId: this.selectedRow.bizId,
                        module: this.selectedRow.bizModule,
                        document: this.selectedRow.bizDocument
                    })
                }
            ],
        };
    },
    computed: {
        endRow() {
            return this.firstRow + this.pageSize;
        },
        /**
         * A map of the column definitions keyed 
         * on the 'field' property.
         * 
         * Additionally calculate a dataType property
         * used to determine which filter operators
         * are shown.
         */
        columnDefinitionsMap() {

            // Map from the type to 'dataType'
            // LHS: skyve attribute type
            // RHS: the dataType value on the Column, determines the 
            // comparison operators available
            const columnDataTypesMap = {
                boolean: 'boolean',
                numeric: 'numeric',
                date: 'date',
                dateTime: 'date',
                timestamp: 'date',
                time: 'date',
                enum: 'text'
            };

            const columnMap = new Map();
            this.columns.forEach(columnDefinition => {

                const defCopy = Object.assign({}, columnDefinition);

                // Modify properties as needed
                // Default type to 'text' if not mapped above
                const dataType = columnDataTypesMap[defCopy.type] ?? 'text'
                defCopy.dataType = dataType;

                columnMap.set(defCopy.field, defCopy);
            });

            return columnMap;
        },
        visibleColumns() {

            // Calculate which columns are visible
            let showPredicate;
            if (this.selectedColumns == null || this.selectedColumns.length == 0) {
                // All columns if nothing is chosen
                showPredicate = (col) => true;
            } else {
                // Or only the selected column
                const shownColumns = this.selectedColumns.map(sc => sc.field);
                showPredicate = (col) => shownColumns.includes(col.field);
            }

            // Removing hidden columns
            const visCols = [...this.columnDefinitionsMap.values()].filter(showPredicate);

            if (this.columnOrder.length > 0) {
                // Sort the columns usin g
                visCols.sort((a, b) => {

                    const aPosn = this.columnOrder.indexOf(a.field);
                    const bPosn = this.columnOrder.indexOf(b.field);
                    if (aPosn == -1 && bPosn == -1) {
                        // Neither column appears in the DataTable's
                        // columnOrder
                        return 0;
                    }

                    if (aPosn == -1) {
                        // Put 'a' after 'b'
                        return 1;
                    }

                    if (bPosn == -1) {
                        // Put 'a' before 'b'
                        return -1;
                    }

                    return aPosn - bPosn;
                });
            }

            return visCols;
        },
        dataSource() {
            if (this.query) {
                return `${this.owningModule}_${this.query}`;
            } else if (this.model) {
                return `${this.owningModule}_${this.owningDocument}__${this.model}`;
            } else {
                return `${this.owningModule}_${this.drivingDocument}`;
            }
        },
        fetchFormData() {
            // Constuct the FormData object that will be POSTed

            const fd = new FormData();
            fd.append('_operationType', 'fetch');
            fd.append('_dataSource', this.dataSource);
            fd.append('_startRow', this.firstRow);
            fd.append('_endRow', this.endRow);

            if (!!this.summarySelection) {
                fd.append('_summary', this.summarySelection);
            }

            if (!!this.contextId) {
                fd.append('_c', this.contextId);
            }

            for (let sortCol of this.sortColumns) {
                fd.append('_sortBy', sortCol);
            }

            if (!! this.smartClientCriteria) {
                fd.append('criteria', JSON.stringify(this.smartClientCriteria));
            }
            else if (this.skyveCriteria.length > 0) {
                fd.append('_constructor', 'AdvancedCriteria');
                fd.append('operator', this.selectedTopLevelOperator);

                for (let crit of this.skyveCriteria) {
                    fd.append('criteria', JSON.stringify(crit));
                }
            }

            return fd;
        },
        sortColumns: {
            get() {
                // convert from the DataTable's format eg:
                // [ { "field": "longInteger", "order": 1 }, { "field": "normalInteger", "order": -1 } ]
                // to what we'll send, eg:
                // [ "longInteger", "-normalInteger" ]
                return this.multiSortMeta
                    .map(col => `${col.order == 1 ? '' : '-'}${col.field}`);
            },
            set(newValue) {
                // reverse of the above
                this.multiSortMeta = newValue.map(str => ({
                    field: str.replace(/^-/, ''),
                    order: str.charAt(0) == '-' ? -1 : 1
                }));
            }
        },
        skyveCriteria() {
            // Convert from the DataTable's 'filter' property
            // to something we can send to Skyve

            let criteria = [];

            for (let columnFilter of Object.entries(this.filters)) {

                const columnName = columnFilter[0];
                const { operator, constraints } = columnFilter[1];

                // Ignore contstraints with empty/nullish value
                const nonNullConstraints = constraints.filter(con => (con.value ?? '') !== '');

                const createCriteria = (constraint) => ({
                    'fieldName': columnName,
                    'value': constraint.value,
                    'operator': constraint.matchMode
                });

                if (nonNullConstraints.length == 1) {
                    // One constraint for this column
                    const crit = createCriteria(nonNullConstraints[0])
                    criteria.push(crit);
                } else if (nonNullConstraints.length > 1) {
                    // Multiple constraints for this column

                    const groupCriteria = {
                        "_constructor": "AdvancedCriteria",
                        "operator": operator,
                        "criteria": []
                    };

                    // Create one criteria for each the user entered
                    // and smush them together into `groupCriteria`
                    nonNullConstraints
                        .map(createCriteria)
                        .forEach(c => groupCriteria.criteria.push(c));

                    criteria.push(groupCriteria);
                }
            }

            return criteria;
        },
        snapshotState() {

            const visibleColNames = this.selectedColumns.map(col => col.field);

            return {
                "filters": this.filters,
                "smartClientCriteria": this.smartClientCriteria,
				"operator": this.selectedTopLevelOperator,
                "visibleColumns": visibleColNames,
                "summarySelection": this.summarySelection,
                "sortColumns": this.sortColumns,
                "columnWidths": this.columnWidths
            };
        },
    },
    methods: {

        notUsed() {
            // Some dumb nonsense to prevent the transpilation process
            // from using $ as a variable name and blatting jQuery
            // There's potentially other globals at risk like this too
            const dontDelete = $;
        },
        /**
         * Call load after a slight delay, cancelling any pending
         * calls.
         */
        debouncedLoad() {
            this.loading = true;
            const delay = 250;
            if (this.loadTimeout) {
                clearTimeout(this.loadTimeout);
            }
            this.loadTimeout = setTimeout(() => {
                this.load();
                this.loadTimeout = null;
            }, delay);
        },
        async load() {

            const listRequest = new Request('./smartlist', {
                method: 'POST',
                body: new URLSearchParams(this.fetchFormData),
                headers: {
                    'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'
                }
            });
            const response = await fetch(listRequest);
            let payload = await response.json();
            if (payload?.response?.status == -1) {
                const errorResponse = payload?.response?.data;
                console.error('Something went wrong retrieving list contents', payload);
                this.errorText = errorResponse;
                this.loading = false;
                throw new Error('Error loading grid contents');
            }

            this.totalRecords = payload.response.totalRows;

            const rows = payload.response.data;

            if (rows.length > 0) {

                if (this.summarySelection == "Count") {
                    rows.at(-1).skipConverters = true;
                }

                // Create column index, FIXME
                const columnDefMap = new Map();
                this.columns.forEach(def => columnDefMap.set(def.field, def));

                rows.filter(row => !row.skipConverters)
                    .forEach(row => applyConverters(row, columnDefMap));
            }

            if (!!this.summarySelection) {
                // Summary row will be the last one, set it aside
                this.summaryRow = rows.pop();
            } else {
                // Clear the summary row
                this.summaryRow = {};
            }

            this.value = rows;
            this.loading = false;
        },
        /**
         * Grab an item from storage (local or session whichever the 
         * DataTable is using).
         * 
         * @param {*} keyPrefix Prefix to add to key. Emtpy string
         * will be the DataTable's state.
         */
        getStorageItem(keyPrefix) {
            const dt = this.$refs.datatable;
            const stateKey = dt.stateKey;

            const storageLoc = dt.stateStorage == 'session' ? sessionStorage : localStorage;
            return storageLoc.getItem(keyPrefix + '' + stateKey);
        },
        setStorageItem(keyPrefix, strValue) {
            const dt = this.$refs.datatable;
            const stateKey = dt.stateKey;

            const storageLoc = dt.stateStorage == 'session' ? sessionStorage : localStorage;
            return storageLoc.setItem(keyPrefix + '' + stateKey, strValue);
        },
        stateSave(event) {
            // There doesn't appear to be any way to grab
            // these values except when the state is saved

            // Datatable's state-save may be triggered as a result 
            // of either of these changes causing reactive recursion 
            // here; using arraysEqual to avoid assigning if nothing
            // has changed.
            const newColumnOrder = event.columnOrder ?? [];
            if (!arraysEqual(newColumnOrder, this.columnOrder)) {
                this.columnOrder = newColumnOrder;
            }

            // Doco is lying about type of columnWidths
            const newWidths = event.columnWidths.split(',').map(s => Number.parseInt(s));
            if (!arraysEqual(newWidths, this.columnWidths)) {
                this.columnWidths = newWidths;
            }
        },
        stateRestore(event) {

            // Triggered when the primevue datatable restores its own state
            // copy out the props 
            this.firstRow = event.first ?? 0;
            this.pageSize = event.rows ?? 25;
            this.sortColumn = event.sortField ?? '';
            this.filters = event.filters ?? {};
            this.smartClientCriteria = event.smartClientCriteria;
        },
        /**
         * Initialise/clear the filter state, optionally setting some 
         * filter state (overlaying the provided state on the just reset state).
         */
        initFilters(incomingFilters = {}) {

            const defaultFilters = {};

            // Create a default entry in 'filters' for each column
            for (let col of this.columns) {
                defaultFilters[col.field] = {
                    operator: FilterOperator.AND,
                    constraints: [{ value: null, matchMode: defaultMatchMode(col.type) }]
                };
            }

            this.filters = Object.assign(defaultFilters, incomingFilters);
            this.smartClientCriteria = null;
        },
        snapshotChanged(newSnapshot) {

            const snapstate = newSnapshot?.snapshot;
            this.setStorageItem(SNAP_KEY_PREFIX, newSnapshot?.bizId);

            if (snapstate) {
                // Filters
                const incomingFilters = snapstate.filters ?? {};
                this.initFilters(incomingFilters);
                this.smartClientCriteria = snapstate.smartClientCriteria;

				this.selectedTopLevelOperator = snapstate.operator ?? 'and';
				
                // Visible columns
                const visibleCols = snapstate.visibleColumns ?? [];
                this.selectedColumns = [];
                for (let col of this.columns) {
                    if (visibleCols.includes(col.field)) {
                        this.selectedColumns.push(col);
                    }
                }

                // Update the DataTable's column order directly
                this.$refs.datatable.d_columnOrder = visibleCols;

                /*
                // Update the column widths?
                // DOESN'T WORK
                const columnWidths = snapstate.columnWidths ?? [];
                const colWidthString = columnWidths.join(',');
                this.$refs.datatable.columnWidthsState = colWidthString
                */

                // Summary/aggregate row
                this.summarySelection = snapstate.summarySelection ?? '';

                // Sort order and columns
                if (!!snapstate.sortColumns) {
                    this.sortColumns = snapstate.sortColumns;
                } else {
                    // Clearing whatever was set if nothing defined in snapshot
                    this.sortColumns = [];
                }

            } else {
                this.clearedSnapshot();
            }
        },
        clearedSnapshot() {

            this.selectedColumns = [];
            this.summarySelection = '';
            this.sortColumns = [];

            this.$refs.datatable.d_columnOrder = null;

            this.initFilters();
        },
        onRowContextMenu(event) {
            this.$refs.cm.show(event.originalEvent);
        },
        onRowClick(event) {
            if (this.actions.selected) {
				// Don't use the ListGrid loading style here as the view loading indicator is invoked by <remoteCommand/>
				callRemoteCommand(this.actions.selected, {bizId: event.data.bizId});
            }
			else if (this.showZoom) {
                const d = event.data;
			    this.zoomInto(d.bizId, d.bizModule, d.bizDocument);
			}
        },
        zoomInto(bizId, module, document) {
            openDocInSameWindow({
                bizId: bizId,
                module: module,
                document: document
            });
        }
    },
    beforeMount() {
       // Calling init filters from mounted() was
        // triggering this issue: https://github.com/primefaces/primevue/issues/4291
        // Seems like the DataTable will reset its filters to whatever
        // was set when it was mounted so we need to set the defaults earlier
        this.initFilters();
    },
    watch: {
        fetchFormData: {
            handler(newValue, oldValue) {
                // Whenever fetchFormData changes call to server
                this.debouncedLoad();
            },
            deep: true
        }
    }
}
</script>
<template>

    <!-- Right click context menu -->
    <ContextMenu
        ref="cm"
        :model="menuModel"
        @hide="selectedRow = null"
    />

    <!-- Error Dialog for any load errors -->
    <Dialog
        modal
        header="Warning"
        :visible="!!errorText"
        @update:visible="() => this.errorText = ''"
    >
        <span class="flex flex-column gap-5">
            <span class="flex flex-row gap-3">
                <div>
                    <span class="pi pi-exclamation-triangle" />
                </div>
                <div v-html="errorText" />
            </span>
            <div class="flex flex-row justify-content-center">
                <Button
                    type="button"
                    label="Ok"
                    @click="() => this.errorText = ''"
                />
            </div>
        </span>
    </Dialog>

    <DataTable
        ref="datatable"
        dataKey="bizId"
        filterDisplay="menu"
        selectionMode="single"
        :stateKey="dataSource"
        stateStorage="session"
        :rowsPerPageOptions="[5, 10, 25, 50, 75, 100]"
        :lazy="true"
        :value="value"
        :loading="loading"
        :totalRecords="totalRecords"
        :paginator="true"
        :reorderableColumns="true"
        :resizableColumns="true"
        columnResizeMode="expand"
        v-model:first="firstRow"
        v-model:rows="pageSize"
        v-model:filters="filters"
        @state-restore="stateRestore"
        @state-save="stateSave"
        contextMenu
        v-model:contextMenuSelection="selectedRow"
        v-model:selection="selectedRow"
        @row-contextmenu="onRowContextMenu"
        @row-click="onRowClick"
        sortMode="multiple"
        v-model:multiSortMeta="multiSortMeta"
    >
        <template #header>
            <div v-if="title">
                {{ title }}
            </div>
            <div class="flex flex-column md:flex-row gap-2">
                <!-- Multi select for choosing visible columns -->
                <MultiSelect
                    v-model="selectedColumns"
                    :options="columns"
                    optionLabel="header"
                    display="comma"
                    placeholder="Select Columns"
                    :maxSelectedLabels="4"
                    selectedItemsLabel="{0} columns selected"
                    :showToggleAll="true"
                />

                <!-- Snapshot CRUD control -->
                <SnapshotPicker v-if="showSnap"
                    :documentQuery="dataSource"
                    :snapshotState="snapshotState"
                    @snapshotChanged="snapshotChanged"
                    stateStorage="session"
                    :stateKey="dataSource"
                />

                <!-- Match Any/All Operator -->
                <Dropdown v-if="showFilter"
                    v-model="selectedTopLevelOperator"
                    :options="topLevelOperators"
                    optionLabel="label"
                    optionValue="value"
                />
                <div v-if="!! smartClientCriteria" style="height: 50px; padding-top: 16px; text-align: center;">
                    <span class="pi pi-exclamation-triangle" />
                    &nbsp;
                    <span>Snapshot cannot be displayed or updated</span>
                </div>
            </div>
        </template>
        <template #empty> No data found.</template>
        <template #loading>
            <span style="color: var(--primary-color-text); text-shadow: 2px 2px 2px black;">
                Loading data. Please wait.
            </span>
        </template>
        <!-- Column key and field here to ensure the session state is saved correctly -->
        <!-- The width !important ensure that resize gestures don't change the column size -->
        <Column key="_action"
                field="_action"
                :reorderableColumn="false"
                style="width:82px !important">
            <!-- Final column with New Doc & Zoom In controls -->
            <template #header>
                <Button v-if="showAdd"
                    icon="pi pi-plus"
                    @click="() => zoomInto()"
                />
            </template>
            <template #body="{ data }">
                <Button v-if="showZoom"
                    icon="pi pi-chevron-right"
                    @click="() => zoomInto(data.bizId)"
                />
            </template>
        </Column>
        <Column
            v-for="col of visibleColumns"
            :key="col.field"
            :field="col.field"
            :header="col.header"
            :sortable="col.sortable"
            :maxConstraints="20"
            :footer="summaryRow[col.field]"
            :filterMatchModeOptions="matchModes[col.type]"
        >
            <template
                #filter="{ filterModel }"
                v-if="showFilter && col.filterable"
            >
                <span v-if="col.type == 'boolean'">
                    <label :for="'bool-' + col.field">{{ col.header }}</label>
                    <TriStateCheckbox
                        :inputId="'bool-' + col.field"
                        v-model="filterModel.value"
                    />
                </span>
                <Dropdown
                    v-else-if="col.type == 'enum'"
                    v-model="filterModel.value"
                    :options="col.enumValues"
                    optionLabel="label"
                    optionValue="value"
                >
                </Dropdown>
                <DateOnlyCalendar
                    v-else-if="col.type == 'date'"
                    v-model="filterModel.value"
                    :dateFormat="dateFormat"
                />
                <Calendar
                    v-else-if="col.type == 'dateTime'"
                    v-model="filterModel.value"
                    :dateFormat="dateFormat"
                    showTime
                    :hourFormat="hourFormat"
                />
                <Calendar
                    v-else-if="col.type == 'timestamp'"
                    v-model="filterModel.value"
                    :dateFormat="dateFormat"
                    showTime
                    :hourFormat="hourFormat"
                    showSeconds
                    :stepSecond="5"
                />
                <TimeCalendar
                    v-else-if="col.type == 'time'"
                    v-model="filterModel.value"
                    :hourFormat="hourFormat"
                />
                <InputText
                    v-else-if="['text', 'numeric'].includes(col.type)"
                    v-model="filterModel.value"
                    type="text"
                    class="p-column-filter"
                    :placeholder="'Search by ' + col.header"
                />
                <div v-else>
                    Unknown type: {{ col.type }}
                </div>
            </template>
            <template #body="{ data, field }">
                <span v-if="col.type == 'image'">
                    <Image
                        :id="data[field]"
                        :module="data.bizModule"
                        :document="data.bizDocument"
                        :binding="field"
                    />
                </span>
                <span v-else>
                    {{ data[field] }}
                </span>
            </template>
        </Column>
<<<<<<< HEAD
=======
        <!-- Column key and field here to ensure the session state is saved correctly -->
        <!-- The width !important ensure that resize gestures don't change the column size -->
        <Column key="_action"
                field="_action"
                :reorderableColumn="false"
                style="width:82px !important">
            <!-- Final column with New Doc & Zoom In controls -->
            <template #header>
                <Button v-if="showAdd"
                    icon="pi pi-plus"
                    @click="() => zoomInto(null, this.drivingModule, this.drivingDocument)"
                />
            </template>
            <template #body="{ data }">
                <Button v-if="showZoom"
                    icon="pi pi-chevron-right"
                    @click="() => zoomInto(data.bizId, data.bizModule, data.bizDocument)"
                />
            </template>
        </Column>
>>>>>>> refs/remotes/origin/9.4.1
        <template #footer v-if="showSummary">
            <Dropdown
                :pt:wrapper:style="{ maxHeight: 'none' }"
                v-model="summarySelection"
                :options="summaryOpts"
            />
        </template>
    </DataTable>
</template>
<style scoped></style>

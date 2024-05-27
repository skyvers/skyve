<script>
import Column from 'primevue/column';
import { FilterOperator } from 'primevue/api';
import { MatchModes } from './support/MatchModes';
import Dropdown from 'primevue/dropdown';
import { openDocInNewWindow, openDocInSameWindow } from './support/Util';
import { applyConverters } from './support/Converters';

const SNAP_KEY_PREFIX = 'dt-selected-snap-bizId-';

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

export default {
    props: {
        module: String,
        document: String,
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
        }
    },
    data() {
        return {
            loading: true,
            loadTimeout: null,
            value: [],
            totalRecords: 0,
            filters: {},
            firstRow: 0,
            pageSize: 25,

            sortColumn: '',
            sortOrder: 0,

            selectedColumns: [],
            columnOrder: [],
            columnWidths: [],

            summarySelection: '',
            summaryOpts: ['', 'Count', 'Avg', 'Sum', 'Min', 'Max'],
            summaryRow: {},

            snapshotBizId: null,

            matchModes: MatchModes,

            // Support params for row right click context menu:
            selectedRow: null,
            menuModel: [
                {
                    label: 'View Detail',
                    icon: 'pi pi-angle-right',
                    command: () => openDocInSameWindow({
                        bizId: this.selectedRow.bizId,
                        module: this.module,
                        document: this.document
                    })
                },
                {
                    label: 'Popout Detail',
                    icon: 'pi pi-external-link',
                    command: () => openDocInNewWindow({
                        bizId: this.selectedRow.bizId,
                        module: this.module,
                        document: this.document
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
                return `${this.module}_${this.query}`;
            } else if (this.model) {
                return `${this.module}_${this.document}__${this.model}`;
            } else {
                return `${this.module}_${this.document}`;
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

            // Sort column and direction
            if ((this.sortColumn ?? '').trim() != '') {
                const sortPrefix = this.sortOrder == 1 ? '' : '-';
                fd.append('_sortBy', sortPrefix + this.sortColumn);
            }

            if (this.skyveCriteria.length > 0) {
                fd.append('_constructor', 'AdvancedCriteria');
                // FIXME allow changing this top-level operator?
                fd.append('operator', 'and');

                for (let crit of this.skyveCriteria) {
                    fd.append('criteria', JSON.stringify(crit));
                }
            }

            return fd;
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

            const visibleColNames = this.visibleColumns.map(col => col.field);

            return {
                "filters": this.filters,
                "visibleColumns": visibleColNames,
                "summarySelection": this.summarySelection,
                "sortOrder": this.sortOrder,
                "sortColumn": this.sortColumn,
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
                console.error('Something went wrong retrieving list contents', payload);
                throw new Error('Error loading list contents');
            }

            this.totalRecords = payload.response.totalRows;

            const rows = payload.response.data;

            if (rows.length > 0) {

                // Create column index, FIXME
                const columnDefMap = new Map();
                this.columns.forEach(def => columnDefMap.set(def.field, def));

                rows.forEach(row => applyConverters(row, columnDefMap));
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
            this.sortOrder = event.sortOrder ?? 0;
            this.filters = event.filters ?? {};
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
        },
        snapshotChanged(newSnapshot) {

            const snapstate = newSnapshot?.snapshot;
            this.setStorageItem(SNAP_KEY_PREFIX, newSnapshot?.bizId);

            if (snapstate) {

                // Filters
                const incomingFilters = snapstate.filters ?? {};
                this.initFilters(incomingFilters);

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

                // Sort order and column
                if (!!snapstate.sortColumn) {
                    this.sortColumn = snapstate.sortColumn;
                    this.sortOrder = snapstate.sortOrder;
                }

            } else {
                this.clearedSnapshot();
            }
        },
        clearedSnapshot() {

            this.selectedColumns = [];
            this.summarySelection = '';
            this.sortColumn = '';
            this.sortOrder = 0;

            this.$refs.datatable.d_columnOrder = null;

            this.initFilters();
        },
        onRowContextMenu(event) {
            this.$refs.cm.show(event.originalEvent);
        },
        onRowClick(event) {
            this.zoomInto(event.data.bizId);
        },
        zoomInto(bizId) {
            openDocInSameWindow({
                bizId: bizId,
                module: this.module,
                document: this.document
            });
        }
    },
    mounted() {
        this.snapshotBizId = this.getStorageItem(SNAP_KEY_PREFIX);
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

    <ContextMenu
        ref="cm"
        :model="menuModel"
        @hide="selectedRow = null"
    />
    <DataTable
        ref="datatable"
        dataKey="bizId"
        filterDisplay="menu"
        selectionMode="single"
        :stateKey="dataSource"
        stateStorage="session"
        :rowsPerPageOptions="[5, 25, 50, 75, 100]"
        :lazy="true"
        :value="value"
        :loading="loading"
        :totalRecords="totalRecords"
        :paginator="true"
        :reorderableColumns="true"
        :resizableColumns="true"
        v-model:first="firstRow"
        v-model:rows="pageSize"
        v-model:filters="filters"
        v-model:sortField="sortColumn"
        v-model:sortOrder="sortOrder"
        @state-restore="stateRestore"
        @state-save="stateSave"
        contextMenu
        v-model:contextMenuSelection="selectedRow"
        @rowContextmenu="onRowContextMenu"
        @row-click="onRowClick"
    >
        <template #header>
            <div v-if="title">
                {{ title }}
            </div>
            <div class="flex flex-column md:flex-row gap-2">
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
                <SnapshotPicker
                    :documentQuery="dataSource"
                    :snapshotState="snapshotState"
                    @snapshotChanged="snapshotChanged"
                    :initialSelection="snapshotBizId"
                />
            </div>
        </template>
        <template #empty> No data found.</template>
        <template #loading> Loading data. Please wait.</template>
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
                v-if="col.filterable"
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
                        :module="module"
                        :document="document"
                        :binding="field"
                    />
                </span>
                <span v-else>
                    {{ data[field] }}
                </span>
            </template>
        </Column>
        <Column>
            <template #header>
                <Button
                    icon="pi pi-plus"
                    @click="() => zoomInto()"
                />
            </template>
            <template #body="{ data }">
                <Button
                    icon="pi pi-chevron-right"
                    @click="() => zoomInto(data.bizId)"
                />
            </template>
        </Column>
        <template #footer>
            <Dropdown
                :pt:wrapper:style="{ maxHeight: 'none' }"
                v-model="summarySelection"
                :options="summaryOpts"
            />
        </template>
    </DataTable>
</template>
<style scoped></style>

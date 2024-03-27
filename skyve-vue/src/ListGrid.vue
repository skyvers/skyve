<script>
import Column from 'primevue/column';
import { FilterMatchMode, FilterOperator } from 'primevue/api';

/**
 * Map from the skyve attribute type to
 * the default filter operator to use for 
 * that column.
 */
function defaultMatchMode(columnType) {

    const mappings = {
        'numeric': FilterMatchMode.EQUALS,
        'text': FilterMatchMode.CONTAINS,
        'time': FilterMatchMode.DATE_BEFORE,
        'timestamp': FilterMatchMode.DATE_BEFORE,
        'date': FilterMatchMode.DATE_BEFORE,
        'dateTime': FilterMatchMode.DATE_BEFORE
    };

    return (mappings[columnType]) ?? FilterMatchMode.EQUALS;
}

/**
 * Compare the two provided arrays for equality;
 * ie: same length, and contents are equal 
 * accoring to ==.
 */
function arraysEqual(a, b) {

    if (a.length != b.length) {
        return false;
    }

    return a.every((val, index) => val == b[index])
}

/**
 * Maps from the DataTable comparison operators
 * to the operator strings used by Skyve
 */
const operatorMap = {
    // 'text': [
    'startsWith': 'iStartsWith',
    'contains': 'iContains',
    'notContains': 'iNotContains',
    'endsWith': 'iEndsWith',
    'equals': 'iEquals',
    'notEquals': 'iNotEqual',
    // 'numeric': [
    'lt': 'lessThan',
    'lte': 'lessOrEqual',
    'gt': 'greaterThan',
    'gte': 'greaterOrEqual',
    //'date': [
    'dateIs': 'equals',
    'dateIsNot': 'notEqual',
    'dateBefore': 'lessThan',
    'dateAfter': 'greaterThan'
    // boolean ops?, seems to always be 'contains'
};

/**
 * Replace values with their `_display` value, if present
 * 
 * @param {*} row Row to be updated
 */
function updateDisplayValues(row) {
    const targetKeys = Object
        .keys(row)
        .filter(colName => colName.startsWith('_display_'))
        .map(colName => ({
            columnKey: colName.replace('_display_', ''),
            displayKey: colName
        }));

    for (let key of targetKeys) {
        row[key.columnKey] = row[key.displayKey]
    }
};

function createRowMutator(columnDefns) {

    const valueMutators = {
        'dateTime': (val) => new Date(val).toLocaleString(),
        'date': (val) => new Date(val).toLocaleDateString(),
        'timestamp': (val) => new Date(val).toLocaleString(),
        'enum': (val, colDefn) => {
            const newLabel = colDefn?.enumValues?.find(entry => entry.value == val)?.label;

            if (!newLabel) {
                // No matching enum value found, use the original value
                // and emit a warning message
                console.warn(`No enum label found for '${val}';`, colDefn);
                return val;
            }

            return newLabel;
        },
    };

    // Index the column definitions on `field`
    // TODO move this back to a computed value
    // roll in any other junk too?
    const columnMap = new Map();
    columnDefns.forEach(def => columnMap.set(def.field, def));

    return (row) => {

        for (let field in row) {
            const columnDef = columnMap.get(field);
            if (!columnDef) continue;
            const type = columnDef.type;

            const valMutator = valueMutators[type] ?? ((v) => v);
            row[field] = valMutator(row[field], columnDef);
        }

        updateDisplayValues(row);
        return row;
    };
}

export default {
    props: {
        module: String,
        query: String,
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
            value: [],
            totalRecords: 0,
            filters: {},
            firstRow: 0,
            pageSize: 25,

            sortColumn: '',
            sortOrder: 0,

            selectedColumns: null,
            columnOrder: [],
            columnWidths: [],

            summarySelection: '',
            summaryOpts: ['', 'Count', 'Avg', 'Sum', 'Min', 'Max'],
            summaryRow: {}
        };
    },
    computed: {
        endRow() {
            return this.firstRow + this.pageSize;
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

            // Mutate the columns prop, removing hidden columns
            // And modifying properties as needed
            // Default type to 'text' if not mapped above
            const visCols = this.columns
                .filter(showPredicate)
                .map(colDefn => {
                    const newType = columnDataTypesMap[colDefn.type] ?? 'text'
                    colDefn.dataType = newType;
                    return colDefn
                });

            if (this.columnOrder.length > 0) {
                visCols.sort((a, b) => this.columnOrder.indexOf(a.field) - this.columnOrder.indexOf(b.field));
            }

            return visCols;
        },
        fetchFormData() {
            // Constuct the FormData object that will be POSTed

            const fd = new FormData();
            fd.append('_operationType', 'fetch');
            fd.append('_dataSource', `${this.module}_${this.query}`);
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
                    'operator': operatorMap[constraint.matchMode]
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
    },
    methods: {
        async load() {
            this.loading = true;

            const listRequest = new Request('../smartlist', {
                method: 'POST',
                body: new URLSearchParams(this.fetchFormData),
                headers: {
                    'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'
                }
            });
            const response = await fetch(listRequest);
            let payload = await response.json();

            this.totalRecords = payload.response.totalRows;

            const rows = payload.response.data;

            if (!!this.summarySelection) {
                // Summary row will be the last one, set it aside
                this.summaryRow = rows.pop();
            } else {
                // Clear the summary row
                this.summaryRow = {};
            }

            if (rows.length > 0) {
                const rowMutator = createRowMutator(this.columns);
                rows.forEach(rowMutator);
            }

            this.value = rows;
            this.loading = false;
        },
        stateRestore(event) {
            // Triggered when the primevue datatable restores its own state
            // copy out the props 
            this.firstRow = event.first;
            this.pageSize = event.rows;
            this.sortColumn = event.sortField;
            this.sortOrder = event.sortOrder;
            this.filters = event.filters;
            // TODO restoring filters blats the defaults
            // created by initFilters(), we should probably
            // do a merge here
        },
        initFilters() {

            this.filters ??= {};

            // Create a default entry in 'filters' for each column
            for (let col of this.columns) {
                if (col.filterable && !this.filters[col.field]) {
                    this.filters[col.field] = {
                        operator: FilterOperator.AND,
                        constraints: [{ value: null, matchMode: defaultMatchMode(col.type) }]
                    };
                }
            }
        },
        stateSave(event) {
            // There doesn't appear to be any way to grab
            // these values except when the state is saved

            // Datatable's state-save may be triggered as a result 
            // of either of these changes causing reactive recursion 
            // here; using arraysEqual to avoid assigning if nothing
            // has changed.
            const newColumnOrder = event.columnOrder ?? [];
            if (!arraysEqual(this.columnOrder, this.columnOrder)) {
                this.columnOrder = newColumnOrder;
            }

            // Doco is lying about type of columnWidths
            const newWidths = event.columnWidths.split(',').map(s => Number.parseInt(s));
            if (!arraysEqual(newWidths, this.columnWidths)) {
                this.columnWidths = newWidths;
            }
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
        fetchFormData(newUrl, oldUrl) {
            // Whenever fetchFormData changes call to server
            this.load();
        }
    }
}
</script>
<template>
    <DataTable
        dataKey="bizId"
        filterDisplay="menu"
        stateStorage="session"
        :rowsPerPageOptions="[5, 25, 50, 75, 100]"
        :lazy="true"
        :value="value"
        :loading="loading"
        :totalRecords="totalRecords"
        :paginator="true"
        :reorderableColumns="true"
        :resizableColumns="true"
        :stateKey="query"
        v-model:first="firstRow"
        v-model:rows="pageSize"
        v-model:filters="filters"
        v-model:sortField="sortColumn"
        v-model:sortOrder="sortOrder"
        @state-restore="stateRestore"
        @state-save="stateSave"
    >
        <template #header>
            <div v-if="title">
                {{ title }}
            </div>
            <div style="text-align:left">
                <MultiSelect
                    v-model="selectedColumns"
                    :options="columns"
                    optionLabel="header"
                    display="chip"
                    placeholder="Select Columns"
                    :showToggleAll="false"
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
            :dataType="col.dataType"
            :footer="summaryRow[col.field]"
        >
            <template
                #filter="{ filterModel }"
                v-if="col.filterable"
            >

                <!-- TODO probably need a label for the booleans -->
                <TriStateCheckbox
                    v-if="col.type == 'boolean'"
                    v-model="filterModel.value"
                />
                <MultiSelect
                    v-else-if="col.type == 'enum'"
                    v-model="filterModel.value"
                    :options="col.enumValues"
                    optionLabel="label"
                    optionValue="value"
                >
                </MultiSelect>
                <Calendar
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
        </Column>
        <template #footer>
            <Dropdown
                v-model="summarySelection"
                :options="summaryOpts"
            />
        </template>
    </DataTable>
</template>
<style scoped></style>

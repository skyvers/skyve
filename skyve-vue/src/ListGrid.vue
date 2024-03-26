<script>
import Column from 'primevue/column';
import { FilterMatchMode, FilterOperator } from 'primevue/api';

// Map from the skyve attribute type to
// the default filter operator to use for
// that column
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

function arraysEqual(a, b) {

    if (a.length != b.length) {
        return false;
    }

    return a.every((val, index) => val == b[index])
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
                fd.append('operator', 'and');

                for (let crit of this.skyveCriteria) {
                    fd.append('criteria', JSON.stringify(crit));
                }
            }

            return fd;
        },
        skyveCriteria() {
            // Convert from the datatables 'filter' object
            // to something we can send to Skyve

            let criteria = [];

            for (let columnFilter of Object.entries(this.filters)) {

                const colName = columnFilter[0];
                const { operator, constraints } = columnFilter[1];

                // Ignore contstraints with empty/nullish value
                const nonNullConstraints = constraints.filter(con => (con.value ?? '') !== '');

                // TODO move this somewhere else
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
                    // TODO boolean ops?, seems to always be 'contains'
                };

                // TODO multiple constraints for one column
                if (nonNullConstraints.length > 0) {
                    const x = nonNullConstraints[0];
                    const crit = {
                        'fieldName': colName,
                        'value': x.value,
                        'operator': operatorMap[x.matchMode]
                    };

                    criteria.push(crit);
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
                this.summaryRow = rows.pop();
            } else {
                this.summaryRow = {};
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
                    this.filters[col.field] = { operator: FilterOperator.AND, constraints: [{ value: null, matchMode: defaultMatchMode(col.type) }] };
                }
            }
        },
        stateSave(event) {
            // There doesn't appear to be any way to grab
            // these values except when the state is saved
            this.columnOrder = event.columnOrder;

            // Doco is lying about type of columnWidths
            const newWidths = event.columnWidths.split(',').map(s => Number.parseInt(s));

            // Datatable's state-save may be triggered when our columnWidths change
            // causing reactive recursion here if we aren't careful
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

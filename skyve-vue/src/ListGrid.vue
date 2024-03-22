<script>
import Column from 'primevue/column';
import { FilterMatchMode, FilterOperator } from 'primevue/api';

export default {
    props: {
        module: String,
        query: String,
        title: String,
        columns: Object
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
        };
    },
    computed: {
        endRow() {
            return this.firstRow + this.pageSize;
        },
        columnsMap() {
            const dtls = new Map();
            for (let column of this.columns) {
                dtls.set(column.field, column);
            }
            return dtls;
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
            // LHS: skyve type, possibly should be a larger
            // range (eg, integer/bizDecimal2/HH24_MI/etc)
            // RHS: the dataType value on the Column, determines the 
            // comparison operators available
            const columnDataTypesMap = {
                boolean: 'boolean',
                numeric: 'numeric',
                date: 'date',
                enum: 'text'
            };

            // Mutate the columns prop, removing hidden columns
            // And modifying properties as needed
            return this.columns
                .filter(showPredicate)
                .map(colDefn => {
                    const newType = columnDataTypesMap[colDefn.type] ?? 'text'
                    colDefn.dataType = newType;
                    return colDefn
                });
        },
        fetchFormData() {

            const fd = new FormData();
            fd.append('_operationType', 'fetch');
            fd.append('_dataSource', `${this.module}_${this.query}`);
            fd.append('_startRow', this.firstRow);
            fd.append('_endRow', this.endRow);

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

                // Ignore contstraints with value == null
                const nonNullConstraints = constraints.filter(con => con.value ?? '' != '');

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
                    // These two shadow some text operators
                    // do we need to send different values to skyve?
                    // 'equals': 'XXXXXX', 
                    // 'notEquals': 'XXXXXX',
                    'lt': 'lessThan',
                    'lte': 'lessOrEqual',
                    'gt': 'greaterThan',
                    'gte': 'greaterOrEqual',
                    //'date': [
                    'dateIs': 'XXXXXX',
                    'dateIsNot': 'XXXXXX',
                    'dateBefore': 'XXXXXX',
                    'dateAfter': 'XXXXXX'
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
        }
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
            this.value = payload.response.data;
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
        },
        initFilters() {

            this.filters ??= {};

            // Create a default entry in 'filters' for each column
            for (let col of this.columns) {
                if (col.filterable && !this.filters[col.field]) {
                    this.filters[col.field] = { operator: FilterOperator.AND, constraints: [{ value: null, matchMode: FilterMatchMode.CONTAINS }] };
                }
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
    <div>{{ filters }}</div>
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
    >
        <template #header>
            <div v-if="title">
                {{ title }}
            </div>
            <div style="text-align:left">
                <MultiSelect v-model="selectedColumns" :options="columns" optionLabel="header" display="chip"
                    placeholder="Select Columns" :showToggleAll="false" />
            </div>
        </template>
        <template #empty> No data found.</template>
        <template #loading> Loading data. Please wait.</template>
        <Column v-for="col of visibleColumns" 
            :key="col.field" 
            :field="col.field" 
            :header="col.header"
            :sortable="col.sortable" 
            :maxConstraints="20"
            :dataType="col.dataType"
            >
            <template 
                #filter="{ filterModel }" 
                v-if="col.filterable">

                <!-- TODO probably need a label for the booleans -->
                <TriStateCheckbox 
                    v-if="col.type == 'boolean'"
                    v-model="filterModel.value" />
                <MultiSelect 
                    v-else-if="col.type == 'enum'"
                    v-model="filterModel.value" 
                    :options="col.enumValues" 
                    optionLabel="label"
                    optionValue="value" >
                </MultiSelect>
                <InputText 
                    v-else
                    v-model="filterModel.value" 
                    type="text" 
                    class="p-column-filter"
                    :placeholder="'Search by ' + col.header" />
            </template>
        </Column>
    </DataTable>

</template>
<style scoped></style>

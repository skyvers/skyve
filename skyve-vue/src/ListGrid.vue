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
        visibleColumns() {

            if (this.selectedColumns == null || this.selectedColumns.length == 0) {
                return this.columns;
            }

            const shownColumns = this.selectedColumns.map(sc => sc.field);
            return this.columns.filter(col => shownColumns.includes(col.field));
        },
        fetchUrl() {

            const paramMap = new Map();
            paramMap.set('_operationType', 'fetch');
            paramMap.set('_dataSource', `${this.module}_${this.query}`);
            paramMap.set('_startRow', this.firstRow);
            paramMap.set('_endRow', this.endRow);

            // If sortColumn is provided append it to the URL
            if ((this.sortColumn ?? '').trim() != '') {
                const sortPrefix = this.sortOrder == 1 ? '' : '-';
                paramMap.set('_sortBy', sortPrefix + this.sortColumn);
            }

            // TODO filter stuff
            if (false) {
                const filter = { "fieldName": "text", "operator": "iContains", "value": "66" };
                const filterStr = encodeURIComponent(JSON.stringify(filter));
                paramMap.set('criteria', filterStr);
                paramMap.set('_constructor', 'AdvancedCriteria');
                paramMap.set('operator', 'and');
            }

            let url = '../smartlist?';
            const params = Array.from(paramMap)
                .map(entry => `${entry[0]}=${entry[1]}`)
                .join('&');

            return url + params;
        }
    },
    methods: {
        async load() {
            this.loading = true;
            const response = await fetch(this.fetchUrl);
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
        }
    },
    mounted() {
        // FIXME Getting two loads when mounting sometimes if the 
        // fetchUrl changes as a result of restoring its state
        this.load();

        // Create a default entry in 'filters' for each column
        for (let col of this.columns) {
            if (col.filterable && !this.filters[col.field]) {
                this.filters[col.field] = { operator: FilterOperator.AND, constraints: [{ value: null, matchMode: FilterMatchMode.CONTAINS }] };
            }
        }
    },
    watch: {
        fetchUrl(newUrl, oldUrl) {
            // Whenever fetchUrl changes call to server
            this.load();
        }
    }
}
</script>
<template>
    <a :href="fetchUrl">{{fetchUrl}}</a>
    <div>filters='{{ filters }}'</div>
    <DataTable 
        :lazy="true" 
        dataKey="bizId" 
        :value="value" 
        :loading="loading" 
        :totalRecords="totalRecords"
        :paginator="true" 
        filterDisplay="menu"
        :reorderableColumns="true" 
        :resizableColumns="true" 
        stateStorage="session" 
        :stateKey="query"
        v-model:first="firstRow" 
        v-model:rows="pageSize" 
        :rowsPerPageOptions="[5, 25, 50, 75, 100]"
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
        <Column v-for="col of visibleColumns" :key="col.field" :field="col.field" :header="col.header"
            :sortable="col.sortable" :maxConstraints="20">
            <template #filter="{ filterModel }" v-if="col.filterable">
                <InputText v-model="filterModel.value" type="text" class="p-column-filter"
                    :placeholder="'Search by ' + col.header" />
            </template>
        </Column>
    </DataTable>
</template>
<style scoped></style>

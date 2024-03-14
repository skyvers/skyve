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
            pageSize: 5,

            selectedColumns: null
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
            console.log('shownColumns', shownColumns);
            const result = this.columns.filter(col => shownColumns.includes(col.field));

            return result;
        }
    },
    methods: {
        async load() {
            this.loading = true;
            const url = `../smartlist?_operationType=fetch&_dataSource=${this.module}_${this.query}&_startRow=${this.firstRow}&_endRow=${this.endRow}`;
            const response = await fetch(url);
            let payload = await response.json();

            this.totalRecords = payload.response.totalRows;
            this.value = payload.response.data;
            this.loading = false;
        },
        onPage(event) {
            this.firstRow = event.first;
            this.load();
        }
    },
    mounted() {
        this.load();

        // Create a default entry in 'filters' for each column
        for (let col of this.columns) {
            if (col.filterable && !this.filters[col.field]) {
                this.filters[col.field] = { operator: FilterOperator.AND, constraints: [{ value: null, matchMode: FilterMatchMode.CONTAINS }] };
            }
        }
    }
}
</script>
<template>
    <DataTable :lazy="true" dataKey="bizId" :value="value" :loading="loading" :totalRecords="totalRecords"
        :paginator="true" :rows="pageSize" @page="onPage($event)" v-model:filters="filters" filterDisplay="menu"
        :reorderableColumns="true" :resizableColumns="true" stateStorage="session" :stateKey="query">
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

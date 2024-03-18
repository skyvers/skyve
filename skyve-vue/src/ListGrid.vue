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

            sortColumn: '',
            sortOrder: '',

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
            return this.columns.filter(col => shownColumns.includes(col.field));
        },
        fetchUrl() {
            let url = `../smartlist?_operationType=fetch&_dataSource=${this.module}_${this.query}&_startRow=${this.firstRow}&_endRow=${this.endRow}`;

            // If sortColumn is provided append it to the URL
            if ((this.sortColumn ?? '').trim() != '') {
                url += `&_sortBy=${this.sortOrder}${this.sortColumn}`;
            }

            return url;
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
        onPage(event) {
            this.firstRow = event.first;
        },
        onSortField(sortColumn) {
            this.sortColumn = sortColumn;
        },
        onSortOrder(sortOrder) {
            this.sortOrder = sortOrder == 1 ? '' : '-';;
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
    <DataTable :lazy="true" dataKey="bizId" :value="value" :loading="loading" :totalRecords="totalRecords"
        :paginator="true" :rows="pageSize" @page="onPage" v-model:filters="filters" filterDisplay="menu"
        :reorderableColumns="true" :resizableColumns="true" stateStorage="session" :stateKey="query"
        @update:sortField="onSortField" @update:sortOrder="onSortOrder">
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

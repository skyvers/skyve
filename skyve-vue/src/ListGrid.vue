<script setup>
  import {ref, onMounted} from 'vue';
  import DataTable from 'primevue/datatable';
  import Column from 'primevue/column';
  import {FilterMatchMode, FilterOperator} from 'primevue/api';

  const loading = ref(true);
  const value = ref(null);
  const totalRecords = ref(0);
  const filters = ref({name: {operator: FilterOperator.AND, constraints: [{value: null, matchMode: FilterMatchMode.STARTS_WITH}]}});

  const _load = async (firstRow) => {

    loading.value = true;

    const endRow = firstRow + 50;
    const url = `../smartlist?_operationType=fetch&_dataSource=${props.module}_${props.query}&_startRow=${firstRow}&_endRow=${endRow}`;

    let response = await fetch(url);
    let payload = await response.json();
    totalRecords.value = payload.response.totalRows;
    value.value = payload.response.data;
    loading.value = false;
  };

  onMounted(() => {
    _load(0);
  });

  const _onPage = (event) => {
    _load(event.first);
  }

  // Try to expose the configure method on for main.js
  defineExpose({
  });

  const props = defineProps({
    module: String,
    query: String,
    title: String,
    columns: {
      type: Object,
      default(rawColumns) {
        return rawColumns;
      }
    }
  });
</script>
<template>
  <DataTable :lazy="true"
                dataKey="bizId"
                :value="value"
                :loading="loading"
                :totalRecords="totalRecords"
                :paginator="true"
                :rows="50"
                @page="_onPage($event)"
                v-model:filters="filters"
                filterDisplay="menu">
    <template #header v-if="title">
      {{title}}
    </template>
    <template #empty> No data found.</template>
    <template #loading> Loading data. Please wait.</template>
    <Column v-for="col of columns" :key="col.field" :field="col.field" :header="col.header" :sortable="col.sortable" :maxConstraints="20">
      <template #filter="{filterModel}" v-if="col.filterable">
        <InputText v-model="filterModel.value" type="text" class="p-column-filter" :placeholder="'Search by ' + col.header" />
      </template>
    </Column>
  </DataTable>
</template>
<style scoped>
</style>

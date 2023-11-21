<script setup>
  import {ref, onMounted} from 'vue';
  import DataTable from 'primevue/datatable';
  import Column from 'primevue/column';
  import {FilterMatchMode, FilterOperator} from 'primevue/api';

  const loading = ref(true);
  const value = ref(null);
  const totalRecords = ref(0);
  const filters = ref({name: {operator: FilterOperator.AND, constraints: [{value: null, matchMode: FilterMatchMode.STARTS_WITH}]}});

  const _load = async (first) => {
    loading.value = true;
    let response = await fetch('../smartlist?_operationType=fetch&_dataSource=' + config.m + '_' + config.q + '&_startRow=' + first + '&_endRow=' + (first + 50));
    let payload = await response.json();
    totalRecords.value = payload.response.totalRows;
    value.value = payload.response.data;
    loading.value = false;
  };

  var config = window.SKYVE.listGridConfig;
  delete window.listGridConfig;

  const configure = (c) => {
    console.log(this);
    config = c;
  };

  onMounted(() => {
    _load(0);
  });

  const _onPage = (event) => {
    _load(event.first);
  }

  // Try to expose the configure method on for main.js
  defineExpose({
    configure
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
    <template #header v-if="config.t">
      {{config.t}}
    </template>
    <template #empty> No data found.</template>
    <template #loading> Loading data. Please wait.</template>
    <Column v-for="col of config.c" :key="col.field" :field="col.field" :header="col.header" :sortable="col.sortable" :maxConstraints="20">
      <template #filter="{filterModel}" v-if="col.filterable">
        <InputText v-model="filterModel.value" type="text" class="p-column-filter" :placeholder="'Search by ' + col.header" />
      </template>
    </Column>
  </DataTable>
</template>
<style scoped>
</style>

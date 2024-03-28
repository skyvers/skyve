<script>

export default {
    props: {
        modelValue: {
            type: String,
        },
        dateFormat: {
            type: String,
            default: 'dd/mm/yy'
        },
    },
    data() {
        return {
            date: null
        };
    },
    computed: {
        dateString() {

            const d = this.date;

            if (!d) return '';

            const month = `${d.getMonth() + 1}`.padStart(2, 0);
            const day = `${d.getDate()}`.padStart(2, 0);

            return `${d.getFullYear()}-${month}-${day}`
        }
    },
    emits: ['update:modelValue'],
    watch: {
        dateString(newVal, oldVal) {
            if (newVal != oldVal) {
                this.$emit('update:modelValue', newVal);
            }
        }
    },
    created() {

        if (!!this.modelValue) {
            this.date = new Date(this.modelValue);
        }
    }
}

</script>
<template>
    <Calendar
        v-model="date"
        :dateFormat="dateFormat"
    />
</template>
<style scoped></style>
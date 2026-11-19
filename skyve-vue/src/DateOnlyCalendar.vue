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

        // The value is yyyy-MM-dd, a day where the user is (new Date() would read it as midnight UTC, the day
        // before west of UTC)
        const match = /^(\d{4})-(\d\d)-(\d\d)$/.exec(this.modelValue ?? '');
        if (match) {
            this.date = new Date(Number(match[1]), Number(match[2]) - 1, Number(match[3]));
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
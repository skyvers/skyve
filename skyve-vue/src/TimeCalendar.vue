<script>

export default {
    props: {
        modelValue: {
            type: String
        },
        hourFormat: {
            type: String,
            default: '24'
        },
        showSeconds: {
            type: Boolean,
            default: false
        }
    },
    data() {
        return {
            date: null
        };
    },
    computed: {
        /**
         * The time as HH:mm:ss, which the server reads whatever the column's converter
         * (the seconds are 00 unless the calendar shows them).
         */
        timeString() {

            if ((this.date ?? '') == '')
                return '';

            const hrs = ('' + this.date.getHours()).padStart(2, '0');
            const mins = ('' + this.date.getMinutes()).padStart(2, '0');
            const secs = this.showSeconds ? ('' + this.date.getSeconds()).padStart(2, '0') : '00';

            return `${hrs}:${mins}:${secs}`;
        }
    },
    emits: ['update:modelValue'],
    watch: {
        timeString(newVal, oldVal) {
            if (newVal != oldVal) {
                this.$emit('update:modelValue', newVal);
            }
        }
    },
    created() {
        const pattern = /(\d\d):(\d\d)(?::(\d\d))?/;
        const match = (this.modelValue ?? '').match(pattern);
        if (match != null) {
            const hrs = match[1];
            const mins = match[2];
            const secs = match[3] ?? 0;

            const newDate = new Date();
            newDate.setHours(hrs, mins, secs);
            this.date = newDate;
        }
    }
}
</script>
<template>
    <Calendar
        v-model="date"
        timeOnly
        showTime
        :hourFormat="hourFormat"
        :showSeconds="showSeconds"
    />
</template>
<style scoped></style>
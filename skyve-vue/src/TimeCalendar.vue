<script>

export default {
    props: {
        modelValue: {
            type: String
        },
        hourFormat: {
            type: String,
            default: '24'
        }
    },
    data() {
        return {
            date: null
        };
    },
    computed: {
        timeString() {

            if ((this.date ?? '') == '')
                return '';

            const hrs = ('' + this.date.getHours()).padStart(2, '0');
            const mins = ('' + this.date.getMinutes()).padStart(2, '0');

            return `${hrs}:${mins}`;
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
        const pattern = /(\d\d):(\d\d)/;
        const match = (this.modelValue ?? '').match(pattern);
        if (match != null) {
            const hrs = match[1];
            const mins = match[2];

            const newDate = new Date();
            newDate.setHours(hrs, mins);
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
    />
</template>
<style scoped></style>
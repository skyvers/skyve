<script>

const params = {
    id: '_n',
    modoc: '_doc',
    binding: '_b',
    width: '_w',
    height: '_h',
};

export default {
    props: {
        id: String,
        module: String,
        document: String,
        size: {
            type: Number,
            default: 64
        },
        binding: String
    },
    data() {
        return {
        };
    },
    computed: {
        modoc() {
            return this.module + '.' + this.document;
        },
        srcUrl() {
            // http://localhost:8080/skyve/content
            // ?_n=2b05e215-fc5a-4591-98f9-edfd10786a6a
            // &_doc=kitchensink.ListAttributes
            // &_b=image
            // &_w=64&_h=64

            const searchParams = new URLSearchParams();
            searchParams.append(params.id, this.id);
            searchParams.append(params.modoc, this.modoc);
            searchParams.append(params.binding, 'image');

            if (!!this.size) {
                searchParams.append(params.width, this.size);
                searchParams.append(params.height, this.size);
            }

            return `content?${searchParams}`;
        }
    },
    methods: {
    }

}

</script>

<template>
    <img
        v-if="!!id"
        :src="srcUrl"
        class="data-grid-image"
    />
</template>

<style scoped>
.data-grid-image {
    max-width: 100px;
    max-height: 100px;
}
</style>
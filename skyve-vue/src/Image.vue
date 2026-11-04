<script>

import { unsanitiseBinding } from './support/Util';

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
        width: {
            type: Number,
            default: 64
        },
        height: {
            type: Number,
            default: 64
        },
        binding: String,
        // Image shown when there is no content (relative to the customer resources)
        emptyThumbnailRelativeFile: {
            type: String,
            default: null
        }
    },
    data() {
        return {
        };
    },
    computed: {
        modoc() {
            return this.module + '.' + this.document;
        },
        contentUrl() {
            const searchParams = new URLSearchParams();
            searchParams.append(params.id, this.id);
            searchParams.append(params.modoc, this.modoc);
            searchParams.append(params.binding, this.rectifiedBinding);
            return `content?${searchParams}`;
        },
        srcUrl() {
            return `${this.contentUrl}&${params.width}=${this.width}&${params.height}=${this.height}`;
        },
        emptyUrl() {
            const searchParams = new URLSearchParams();
            searchParams.append(params.id, this.emptyThumbnailRelativeFile);
            searchParams.append(params.modoc, this.modoc);
            searchParams.append(params.width, this.width);
            searchParams.append(params.height, this.height);
            return `resources?${searchParams}`;
        },
        imageStyle() {
            return {
                width: `${this.width}px`,
                height: `${this.height}px`,
                objectFit: 'contain'
            };
        },
        rectifiedBinding() {
            // Convert from the SmartClient sanitised binding into the
            // original binding value for content servlet
            return unsanitiseBinding(this.binding);
        }
    },
    methods: {
    }

}

</script>

<template>
    <a
        v-if="!!id"
        :href="contentUrl"
        style="border:0"
        target="_blank"
    >
        <img
            :src="srcUrl"
            :style="imageStyle"
        />
    </a>
    <img
        v-else-if="!!emptyThumbnailRelativeFile"
        :src="emptyUrl"
        :style="imageStyle"
    />
</template>
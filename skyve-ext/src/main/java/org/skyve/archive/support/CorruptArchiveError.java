package org.skyve.archive.support;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlEnum;

/**
 * Describes a corrupt archive record captured during archive processing.
 *
 * <p>Implementations are generated document beans stored in the admin module to
 * track corrupt input files, their source archive type, processing time, and
 * current remediation {@link Resolution}.
 */
public interface CorruptArchiveError extends PersistentBean {
    /**
     * Owning module name for this document type.
     */
    public static final String MODULE_NAME = AppConstants.ADMIN_MODULE_NAME;

    /**
     * Document name for this document type.
     */
    public static final String DOCUMENT_NAME = "CorruptArchiveError";

    /**
     * Binding name for the resolution attribute.
     */
    @SuppressWarnings("java:S115") // Property-name constant mirrors generated domain binding names.
    public static final String resolutionPropertyName = "resolution";

    /**
     * Returns the original file name of the corrupt archive payload.
     *
     * @return Corrupt archive file name
     */
    public String getFilename();

    /**
     * Sets the original file name of the corrupt archive payload.
     *
     * @param filename Corrupt archive file name
     */
    public void setFilename(String filename);

    /**
     * Returns the archive type module that the payload was expected to target.
     *
     * @return Archive type module name
     */
    public String getArchiveTypeModule();

    /**
     * Sets the archive type module that the payload was expected to target.
     *
     * @param archiveTypeModule Archive type module name
     */
    public void setArchiveTypeModule(String archiveTypeModule);

    /**
     * Returns the archive type document that the payload was expected to target.
     *
     * @return Archive type document name
     */
    public String getArchiveTypeDocument();

    /**
     * Sets the archive type document that the payload was expected to target.
     *
     * @param archiveTypeDocument Archive type document name
     */
    public void setArchiveTypeDocument(String archiveTypeDocument);

    /**
     * Returns when this corrupt archive record was captured.
     *
     * @return Capture timestamp
     */
    public Timestamp getTimestamp();

    /**
     * Sets when this corrupt archive record was captured.
     *
     * @param timestamp Capture timestamp
     */
    public void setTimestamp(Timestamp timestamp);

    /**
     * Returns the current remediation resolution state.
     *
     * @return Resolution state
     */
    public Resolution getResolution();

    /**
     * Sets the current remediation resolution state.
     *
     * @param resolution Resolution state
     */
    public void setResolution(Resolution resolution);

    /**
     * Defines remediation states for corrupt archive records.
     */
    @XmlEnum
    @Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
    @SuppressWarnings("java:S115") // Generated enum names are stable persisted domain codes.
    public static enum Resolution implements Enumeration {
        /**
         * Corrupt archive has been identified but not yet actioned.
         */
        unresolved("unresolved", "unresolved"),

        /**
         * Investigation or remediation is currently underway.
         */
        inProgress("in_progress", "in_progress"),

        /**
         * Corrupt archive has been successfully resolved.
         */
        resolved("resolved", "resolved"),

        /**
         * Remediation attempted but ultimately failed.
         */
        failed("failed", "failed");

        private String code;
        private String description;

        /** @hidden */
        private DomainValue domainValue;

        /** @hidden */
        private static List<DomainValue> domainValues = Stream.of(values())
                                                              .map(Resolution::toDomainValue)
                                                              .collect(Collectors.toUnmodifiableList());

        private Resolution(String code, String description) {
            this.code = code;
            this.description = description;
            this.domainValue = new DomainValue(code, description);
        }

        /**
         * Returns the persisted code value for this enum constant.
         *
         * @return Stable code used for storage and transport
         */
        @Override
        public String toCode() {
            return code;
        }

        /**
         * Returns a localised display label for this enum constant.
         *
         * @return Localised description
         */
        @Override
        public String toLocalisedDescription() {
            return Util.i18n(description);
        }

        /**
         * Returns this constant as a Skyve domain value pair.
         *
         * @return Domain value for UI/list model usage
         */
        @Override
        public DomainValue toDomainValue() {
            return domainValue;
        }

        /**
         * Resolves a resolution constant from a persisted code.
         *
         * @param code Persisted resolution code
         * @return Matching resolution, or {@code null} when unmatched
         */
        public static Resolution fromCode(String code) {
            Resolution result = null;

            for (Resolution value : values()) {
                if (value.code.equals(code)) {
                    result = value;
                    break;
                }
            }

            return result;
        }

        /**
         * Resolves a resolution constant from a localised description.
         *
         * @param description Localised description text
         * @return Matching resolution, or {@code null} when unmatched
         */
        public static Resolution fromLocalisedDescription(String description) {
            Resolution result = null;

            for (Resolution value : values()) {
                if (value.toLocalisedDescription()
                         .equals(description)) {
                    result = value;
                    break;
                }
            }

            return result;
        }

        /**
         * Returns all resolution values as an immutable list of domain values.
         *
         * @return Immutable list of domain values
         */
        public static List<DomainValue> toDomainValues() {
            return domainValues;
        }
    }
}

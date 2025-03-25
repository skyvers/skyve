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

public interface CorruptArchiveError extends PersistentBean {

    public static final String MODULE_NAME = AppConstants.ADMIN_MODULE_NAME;
    public static final String DOCUMENT_NAME = "CorruptArchiveError";
    public static final String resolutionPropertyName = "resolution";

    public String getFilename();

    public void setFilename(String filename);

    public String getArchiveTypeModule();

    public void setArchiveTypeModule(String archiveTypeModule);

    public String getArchiveTypeDocument();

    public void setArchiveTypeDocument(String archiveTypeDocument);

    public Timestamp getTimestamp();

    public void setTimestamp(Timestamp timestamp);

    public Resolution getResolution();

    public void setResolution(Resolution resolution);

    /**
     * Resolution
     **/
    @XmlEnum
    @Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
    public static enum Resolution implements Enumeration {
        unresolved("unresolved", "unresolved"), inProgress("in_progress", "in_progress"), resolved("resolved",
                "resolved"), failed("failed", "failed");

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

        @Override
        public String toCode() {
            return code;
        }

        @Override
        public String toLocalisedDescription() {
            return Util.i18n(description);
        }

        @Override
        public DomainValue toDomainValue() {
            return domainValue;
        }

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

        public static List<DomainValue> toDomainValues() {
            return domainValues;
        }
    }
}

package org.skyve.impl.metadata.model;

import java.util.Objects;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.document.Interface;

import com.google.common.base.MoreObjects;

import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.XmlValue;

/**
 * JAXB-annotated value type that records a Java interface name declared on a document.
 *
 * <p>A document may specify additional Java interfaces that its generated domain class
 * must implement.  Each such declaration is represented by an {@code InterfaceImpl},
 * which holds the fully-qualified interface name and is unmarshalled from the
 * {@code <implements>} element in the document descriptor XML.
 *
 * <p>Threading: immutable after construction; safe for concurrent reads.
 *
 * @see org.skyve.metadata.model.document.Interface
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class InterfaceImpl implements Interface {
    private static final long serialVersionUID = 5670142647312527597L;

    private String name;

    public void setInterfaceName(String name) {
        this.name = name;
    }

    @XmlValue
    @Override
    public String getInterfaceName() {
        return name;
    }

    @Override
    public int hashCode() {

        return Objects.hash(name);
    }

    @Override
    public boolean equals(Object obj) {

        if (obj instanceof Interface other) {
            return Objects.equals(this.getInterfaceName(), other.getInterfaceName());
        }

        return false;
    }

    @Override
    public String toString() {

        return MoreObjects.toStringHelper(this)
                          .add("name", name)
                          .toString();
    }

}

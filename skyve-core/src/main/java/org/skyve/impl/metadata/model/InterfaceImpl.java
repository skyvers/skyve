package org.skyve.impl.metadata.model;

import java.util.Objects;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.document.Interface;

import com.google.common.base.MoreObjects;

import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.XmlValue;

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

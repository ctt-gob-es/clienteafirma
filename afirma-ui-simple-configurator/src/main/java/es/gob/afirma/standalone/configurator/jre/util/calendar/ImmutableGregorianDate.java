/*
 * Copyright (c) 2005, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package es.gob.afirma.standalone.configurator.jre.util.calendar;

import java.util.Locale;
import java.util.TimeZone;

class ImmutableGregorianDate extends BaseCalendar.Date {
    private final BaseCalendar.Date date;

    ImmutableGregorianDate(final BaseCalendar.Date date) {
        if (date == null) {
            throw new NullPointerException();
        }
        this.date = date;
    }

    @Override
	public Era getEra() {
        return this.date.getEra();
    }

    public CalendarDate setEra(final Era era) {
        unsupported(); return this;
    }

    @Override
	public int getYear() {
        return this.date.getYear();
    }

    @Override
	public CalendarDate setYear(final int year) {
        unsupported(); return this;
    }

    @Override
	public CalendarDate addYear(final int n) {
        unsupported(); return this;
    }

    @Override
	public boolean isLeapYear() {
        return this.date.isLeapYear();
    }

    @Override
	void setLeapYear(final boolean leapYear) {
        unsupported();
    }

    @Override
	public int getMonth() {
        return this.date.getMonth();
    }

    @Override
	public CalendarDate setMonth(final int month) {
        unsupported(); return this;
    }

    @Override
	public CalendarDate addMonth(final int n) {
        unsupported(); return this;
    }

    @Override
	public int getDayOfMonth() {
        return this.date.getDayOfMonth();
    }

    @Override
	public CalendarDate setDayOfMonth(final int date) {
        unsupported(); return this;
    }

    @Override
	public CalendarDate addDayOfMonth(final int n) {
        unsupported(); return this;
    }

    @Override
	public int getDayOfWeek() {
        return this.date.getDayOfWeek();
    }

    @Override
	public int getHours() {
        return this.date.getHours();
    }

    @Override
	public CalendarDate setHours(final int hours) {
        unsupported(); return this;
    }

    @Override
	public CalendarDate addHours(final int n) {
        unsupported(); return this;
    }

    @Override
	public int getMinutes() {
        return this.date.getMinutes();
    }

    @Override
	public CalendarDate setMinutes(final int minutes) {
        unsupported(); return this;
    }

    @Override
	public CalendarDate addMinutes(final int n) {
        unsupported(); return this;
    }

    @Override
	public int getSeconds() {
        return this.date.getSeconds();
    }

    @Override
	public CalendarDate setSeconds(final int seconds) {
        unsupported(); return this;
    }

    @Override
	public CalendarDate addSeconds(final int n) {
        unsupported(); return this;
    }

    @Override
	public int getMillis() {
        return this.date.getMillis();
    }

    @Override
	public CalendarDate setMillis(final int millis) {
        unsupported(); return this;
    }

    @Override
	public CalendarDate addMillis(final int n) {
        unsupported(); return this;
    }

    @Override
	public long getTimeOfDay() {
        return this.date.getTimeOfDay();
    }

    @Override
	public CalendarDate setDate(final int year, final int month, final int dayOfMonth) {
        unsupported(); return this;
    }

    @Override
	public CalendarDate addDate(final int year, final int month, final int dayOfMonth) {
        unsupported(); return this;
    }

    @Override
	public CalendarDate setTimeOfDay(final int hours, final int minutes, final int seconds, final int millis) {
        unsupported(); return this;
    }

    @Override
	public CalendarDate addTimeOfDay(final int hours, final int minutes, final int seconds, final int millis) {
        unsupported(); return this;
    }

    @Override
	protected void setTimeOfDay(final long fraction) {
        unsupported();
    }

    @Override
	public boolean isNormalized() {
        return this.date.isNormalized();
    }

    @Override
	public boolean isStandardTime() {
        return this.date.isStandardTime();
    }

    @Override
	public void setStandardTime(final boolean standardTime) {
        unsupported();
    }

    @Override
	public boolean isDaylightTime() {
        return this.date.isDaylightTime();
    }

    @Override
	protected void setLocale(final Locale loc) {
        unsupported();
    }

    @Override
	public TimeZone getZone() {
        return this.date.getZone();
    }

    @Override
	public CalendarDate setZone(final TimeZone zoneinfo) {
        unsupported(); return this;
    }

    @Override
	public boolean isSameDate(final CalendarDate date) {
        return date.isSameDate(date);
    }

    @Override
	public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof ImmutableGregorianDate)) {
            return false;
        }
        return this.date.equals(((ImmutableGregorianDate) obj).date);
    }

    @Override
	public int hashCode() {
        return this.date.hashCode();
    }

    @Override
	public Object clone() {
        return super.clone();
    }

    @Override
	public String toString() {
        return this.date.toString();
    }

    @Override
	protected void setDayOfWeek(final int dayOfWeek) {
        unsupported();
    }

    @Override
	protected void setNormalized(final boolean normalized) {
        unsupported();
    }

    @Override
	public int getZoneOffset() {
        return this.date.getZoneOffset();
    }

    @Override
	protected void setZoneOffset(final int offset) {
        unsupported();
    }

    @Override
	public int getDaylightSaving() {
        return this.date.getDaylightSaving();
    }

    @Override
	protected void setDaylightSaving(final int daylightSaving) {
        unsupported();
    }

    @Override
	public int getNormalizedYear() {
        return this.date.getNormalizedYear();
    }

    @Override
	public void setNormalizedYear(final int normalizedYear) {
        unsupported();
    }

    private void unsupported() {
        throw new UnsupportedOperationException();
    }
}

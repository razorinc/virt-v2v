# -*-Shell-script-*-
echo "Starting Kickstart Post"
PATH=/sbin:/usr/sbin:/bin:/usr/bin
export PATH

# cleanup rpmdb to allow non-matching host and chroot RPM versions
rm -f /var/lib/rpm/__db*

echo "Creating shadow files"
# because we aren't installing authconfig, we aren't setting up shadow
# and gshadow properly.  Do it by hand here
pwconv
grpconv

echo "Forcing C locale"
# force logins (via ssh, etc) to use C locale, since we remove locales
cat >> /etc/profile << \EOF
# force our locale to C since we don't have locale stuff'
export LC_ALL=C LANG=C
EOF

# remove errors from /sbin/dhclient-script
DHSCRIPT=/sbin/dhclient-script
sed -i 's/mv /cp -p /g'  $DHSCRIPT
sed -i '/rm -f.*${interface}/d' $DHSCRIPT
sed -i '/rm -f \/etc\/localtime/d' $DHSCRIPT
sed -i '/rm -f \/etc\/ntp.conf/d' $DHSCRIPT
sed -i '/rm -f \/etc\/yp.conf/d' $DHSCRIPT

# Lock root account
#passwd -l root

#strip out all unncesssary locales
localedef --list-archive | grep -v -i -E 'en_US.utf8' |xargs localedef --delete-from-archive
mv /usr/lib/locale/locale-archive /usr/lib/locale/locale-archive.tmpl
/usr/sbin/build-locale-archive

# Run virt-p2v
cat >> /etc/rc.local <<'EOF'

Xlog=/tmp/X.log
again=$(mktemp)

while [ -f "$again" ]; do
    /usr/bin/xinit /usr/bin/virt-p2v-launcher > $Xlog 2>&1

    # virt-p2v-launcher will have touched this file if it ran
    if [ -f /tmp/virt-p2v-launcher ]; then
        rm $again
        break
    fi

    /usr/bin/openvt -sw -- /bin/bash -c "
echo virt-p2v-launcher failed
select c in \
    \"Try again\" \
    \"Debug\" \
    \"Power off\"
do
    if [ \"\$c\" == Debug ]; then
        echo Output was written to $Xlog
        echo Exit this shell to run virt-p2v-launcher again
        bash -l
    elif [ \"\$c\" == \"Power off\" ]; then
        rm $again
    fi
    break
done
"

done
/sbin/poweroff
EOF

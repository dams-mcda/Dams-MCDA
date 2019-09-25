from django.contrib import admin
from .models import DamsMCDAGroup, DamsMCDAUser, DamsMCDARunPreference

# Register your models here.
admin.site.register(DamsMCDAUser)
admin.site.register(DamsMCDAGroup)
admin.site.register(DamsMCDARunPreference)

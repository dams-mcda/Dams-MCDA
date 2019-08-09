from django.contrib import admin
from .models import DamsMCDAGroup, DamsMCDAUser

# Register your models here.
admin.site.register(DamsMCDAUser)
admin.site.register(DamsMCDAGroup)
